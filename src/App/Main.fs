module Fable.Repl.Main

open Fable.Core
open Fable.Core.JsInterop
open Fable.FontAwesome
open Browser.Types
open Browser
open Fetch.Types
open Elmish
open Thoth.Elmish
open Prelude
open Editor
open Mouse
open Thoth.Json
open Fable.WebWorker

open Feliz
open Feliz.Bulma
open Feliz.ReactEditor

type ISavedState =
    abstract code: string
    abstract html: string
    abstract css: string

let private Worker(): Worker =
    // importDefault "worker-loader!../../../Fable/src/fable-standalone/src/Worker/Worker.fsproj"
    Worker.Create(Literals.WORKER_BUNDLE_URL)

let private loadState(_key: string): ISavedState = importMember "./js/util.js"
let private saveState(_key: string, _code: string, _html: string, _cssCode : string): unit = importMember "./js/util.js"
let private updateQuery(_fsharpCode : string, _htmlCode : string, _cssCode : string): unit = importMember "./js/util.js"

type IEditor = Monaco.Editor.IStandaloneCodeEditor

type State =
    | Loading
    | Idle
    | Compiling
    | Compiled

[<RequireQualifiedAccess>]
type OutputTab =
    | Code
    | Live

[<RequireQualifiedAccess>]
type CodeTab =
    | FSharp
    | Html
    | Css

type DragTarget =
    | NoTarget
    | PanelSplitter

type EditorCollapse =
    | BothExtended
    | HtmlOnly
    | FSharpOnly

type Model =
    { 
        FSharpEditor: IEditor
        Worker: ObservableWorker<WorkerAnswer>
        State: State
        IFrameUrl : string
        OutputTab : OutputTab
        CodeTab : CodeTab
        CodeES2015: string
        FSharpCode : string
        FSharpErrors : Monaco.Editor.IMarkerData[]
        HtmlCode: string
        CssCode: string
        DragTarget : DragTarget
        PanelSplitRatio : float
        Sidebar : Sidebar.Model
        IsProblemsPanelExpanded : bool
        Logs : ConsolePanel.Log list 
    }

type EndCompileStatus = Result<string * Fable.Standalone.Error[], string>

type Msg =
    | SetFSharpEditor of IEditor
    | LoadSuccess
    | LoadFail
    | ParseEditorCode
    | Reset
    | UrlHashChange
    | GistLoaded of string*string*string
    | LoadGistError of exn
    | LoadGist of string
    | MarkEditorErrors of Fable.Standalone.Error[]
    | StartCompile of string option
    | EndCompile of EndCompileStatus
    | UpdateStats of CompileStats
    | ShareableUrlReady of unit
    | GistUrlReady of string
    | ShareGistError of exn
    | NoToken
    | SetOutputTab of OutputTab
    | SetCodeTab of CodeTab
    | ToggleProblemsPanel
    | SetIFrameUrl of string
    | PanelDragStarted
    | PanelDrag of Position
    | PanelDragEnded
    | MouseUp
    | MouseMove of Mouse.Position
    | AddConsoleLog of string
    | AddConsoleError of string
    | AddConsoleWarn of string
    | SidebarMsg of Sidebar.Msg
    | ChangeFsharpCode of string
    | ChangeHtmlCode of string
    | ChangeCssCode of string
    | UpdateQueryFailed of exn
    | RefreshIframe

// Custom error message
let errorString (response: Response) =
    string response.Status + " " + response.StatusText + " for URL " + response.Url

let fetchAs<'T> (url: string) (decoder: Decoder<'T>) (init: RequestProperties list) =
    promise {
        let! response = GlobalFetch.fetch(RequestInfo.Url url, Fetch.requestProps init)
        if not response.Ok then
            return errorString response |> failwith
        else
            let! txt = response.text()
            return
                match Decode.fromString decoder txt with
                | Ok res -> res
                | Error er -> failwith er
    }

let private addLog log (model : Model) =
    { model with 
        Logs =
            if model.Logs.Length >= Literals.MAX_LOGS_LENGTH then
                model.Logs.Tail @ [log]
            else
                model.Logs @ [log] 
    }

let private generateHtmlUrl (model: Model) jsCode =
    saveState(Literals.STORAGE_KEY, model.FSharpCode, model.HtmlCode, model.CssCode)
    Generator.generateHtmlBlobUrl model.HtmlCode model.CssCode jsCode

let private clamp min max value =
    if value >= max
    then max
    elif value <= min
    then min
    else value

let private postToGist =
    let decoder = Decode.object (fun get -> get.Required.Field "id" Decode.string)
    let toContent str = Encode.object ["content", Encode.string str]
    fun (token,code,html,css) ->
        promise {
            let data =
              Encode.object [
                "public", Encode.bool true
                "description", Encode.string "Created with Fable REPL"
                "files", Encode.object [
                    yield "fable-repl.fs", toContent code
                    yield "fable-repl.html", toContent html
                    if (css:string).Trim() <> "" then
                       yield "fable-repl.css", toContent css
                ] ] |> Encode.toString 0

            return! fetchAs "https://api.github.com/gists" decoder
                [ RequestProperties.Method HttpMethod.POST
                  RequestProperties.Body !^data
                  Fetch.requestHeaders [HttpRequestHeaders.Authorization ("token " + token)]
                ]
    }

let private loadGist =
    let recover choice =
        promise {
            match choice with
            | Choice1Of2 url ->
                return! fetchAs url Decode.string []
            | Choice2Of2 content ->
                return content }
    let innerDecoder =
        Decode.object (fun get ->
            if get.Required.Field "truncated" Decode.bool then
                get.Required.Field "raw_url" Decode.string |> Choice1Of2
            else
                get.Required.Field "content" Decode.string |> Choice2Of2)

    let decoder =
        Decode.object (fun get ->
            get.Required.At ["files";"fable-repl.fs"] innerDecoder,
            get.Required.At ["files";"fable-repl.html"] innerDecoder,
            get.Optional.At ["files";"fable-repl.css"] innerDecoder)
    fun gist ->
        let url = "https://api.github.com/gists/" + gist
        promise {
            let! (code,html,css) = fetchAs url decoder []
            let! code = recover code
            let! html = recover html
            let! css = css |> Option.map recover |> Option.defaultValue (Promise.lift "")
            return (code, html, css) 
        }

let private showGlobalErrorToast msg =
    Toast.message msg
    |> Toast.title "Failed to compile"
    |> Toast.position Toast.BottomRight
    |> Toast.icon Fa.Solid.Exclamation
    |> Toast.noTimeout
    |> Toast.withCloseButton
    |> Toast.error

let update msg (model : Model) =
    match msg with
    | LoadSuccess ->
        let activateParsing dispatch =
            let obs = createObservable(fun trigger ->
                model.FSharpEditor.getModel().onDidChangeContent(fun _ -> trigger()) |> ignore)
            debounce 1000 obs
            |> Observable.add (fun () -> dispatch ParseEditorCode)
            obs.Trigger() // Trigger a first parse

        let browserAdviceCommand =
            if not ReactDeviceDetect.exports.isChrome
                && not ReactDeviceDetect.exports.isSafari then

                Toast.message "We recommend using Chrome or Safari, for best performance"
                |> Toast.icon Fa.Solid.Info
                |> Toast.position Toast.BottomRight
                |> Toast.timeout (System.TimeSpan.FromSeconds 5.)
                |> Toast.dismissOnClick
                |> Toast.info

            else
                Cmd.none

        { model with 
            State = Idle 
        }
        , Cmd.batch [ 
            [ activateParsing ]
            Cmd.ofMsg (StartCompile None)
            browserAdviceCommand 
        ]

    | LoadFail ->
        let msg = "Assemblies couldn't be loaded. Some firewalls prevent download of binary files, please check."
        { model with 
            State = Idle 
        }
        , showGlobalErrorToast msg

    | ParseEditorCode ->
        let content = model.FSharpEditor.getModel().getValue (Monaco.Editor.EndOfLinePreference.TextDefined, true)
        ParseCode(content, model.Sidebar.Options.ToOtherFSharpOptions) |> model.Worker.Post
        model
        , Cmd.none

    | SetFSharpEditor ed -> 
        { model with 
            FSharpEditor = ed 
        }
        , Cmd.none

    | ToggleProblemsPanel ->
        { model with 
            IsProblemsPanelExpanded = not model.IsProblemsPanelExpanded 
        }
        , Cmd.none

    | LoadGist gist ->
        model
        , Cmd.OfPromise.either loadGist gist GistLoaded LoadGistError

    | Reset ->
        window.localStorage.removeItem(Literals.STORAGE_KEY)
        let saved = loadState(Literals.STORAGE_KEY)
        { model with 
            FSharpCode = saved.code
            HtmlCode = saved.html
            CssCode = saved.css
            CodeES2015 = ""
            IFrameUrl = ""
            Logs = [] 
        }
        , Router.newUrl Router.Home

    | GistLoaded (code, html, css) ->
        { model with 
            FSharpCode = code
            HtmlCode = html
            CssCode = css 
        }
        , Cmd.ofMsg (StartCompile (Some code))

    | GistUrlReady gist ->
        model
        , Cmd.batch [
            Router.modifyUrl (Router.LoadGist (Some gist))
            Cmd.ofMsg (ShareableUrlReady ()) 
        ]

    | ShareGistError exn ->
        JS.console.error exn

        model
        , Toast.message "An error occured when creating the gist. Is the token valid?"
            |> Toast.icon Fa.Solid.ExclamationTriangle
            |> Toast.position Toast.BottomRight
            |> Toast.warning

    | NoToken ->
        model
        , Toast.message "You need to register your GitHub API token before sharing to Gist"
            |> Toast.icon Fa.Solid.ExclamationTriangle
            |> Toast.position Toast.BottomRight
            |> Toast.warning

    | UrlHashChange ->
        let parsed = loadState(Literals.STORAGE_KEY)
        { model with 
            FSharpCode = parsed.code
            HtmlCode = parsed.html
            CssCode = parsed.css 
        }
        , Cmd.ofMsg (StartCompile (Some parsed.code))

    | MarkEditorErrors errors ->
        { model with 
            FSharpErrors = mapErrorToMarker errors 
        }
        , Cmd.none

    | StartCompile code ->
        match model.State with
        | Loading | Compiling -> 
            model
            , Cmd.none

        | Compiled | Idle ->
            let code =
                match code with
                | Some code -> code
                | None -> model.FSharpCode
            let opts = model.Sidebar.Options
            CompileCode(code, opts.ToOtherFSharpOptions) |> model.Worker.Post
            
            { model with 
                State = Compiling 
            }
            , Cmd.none

    | EndCompile result ->
        match result with
        | Ok(codeES2015, errors) ->
            let hasCriticalErrors = errors |> Array.exists (fun e -> not e.IsWarning)
            if hasCriticalErrors then
                let toastCmd =
                    Toast.message "Failed to compile"
                    |> Toast.position Toast.BottomRight
                    |> Toast.icon Fa.Solid.Exclamation
                    |> Toast.dismissOnClick
                    |> Toast.error
                
                { model with 
                    State = Compiled
                    FSharpErrors = mapErrorToMarker errors
                    Logs = [ ConsolePanel.Log.Separator ] 
                }
                , toastCmd

            else
                let toastCmd =
                    Toast.message "Compiled successfuly"
                    |> Toast.position Toast.BottomRight
                    |> Toast.icon Fa.Solid.Check
                    |> Toast.dismissOnClick
                    |> Toast.success
                
                { model with 
                    CodeES2015 = codeES2015
                    State = Compiled
                    FSharpErrors = mapErrorToMarker errors
                    Logs = [ ConsolePanel.Log.Separator ] 
                }
                , Cmd.batch [ 
                    Cmd.OfFunc.perform (generateHtmlUrl model) codeES2015 SetIFrameUrl
                    toastCmd 
                ]

        | Error msg ->
            { model with 
                State = Compiled 
            }
            , showGlobalErrorToast msg

    | SetIFrameUrl newUrl ->
        { model with 
            IFrameUrl = newUrl 
        }
        , Cmd.none

    | SetOutputTab newTab ->
        { model with 
            OutputTab = newTab 
        }
        , Cmd.none

    | SetCodeTab newTab ->
        { model with 
            CodeTab = newTab 
        }
        , Cmd.none

    | MouseUp ->
        let cmd =
            match model.DragTarget with
            | NoTarget -> Cmd.none
            | PanelSplitter ->
                Cmd.ofMsg PanelDragEnded
        
        model
        , cmd

    | MouseMove position ->
        let cmd =
            match model.DragTarget with
            | NoTarget -> Cmd.none
            | PanelSplitter ->
                Cmd.ofMsg (PanelDrag position)
        
        model
        , cmd

    | PanelDragStarted ->
        { model with 
            DragTarget = PanelSplitter 
        }
        , Cmd.none

    | PanelDragEnded ->
        { model with 
            DragTarget = NoTarget 
        }
        , Cmd.none

    | PanelDrag position ->
        let offset =
            if model.Sidebar.IsExpanded then 250. else 0.
        let splitRatio =
            position
            |> (fun p -> p.X - offset)
            |> (fun w -> w / (window.innerWidth - offset))
            |> clamp 0.2 0.8
        // printfn "PANELDRAG: x %f offset %f innerWidth %f splitRatio %f"
        //     position.X offset window.innerWidth splitRatio
        { model with 
            PanelSplitRatio = splitRatio 
        }
        , Cmd.none

    | SidebarMsg msg ->
        let (subModel, cmd, externalMsg) = Sidebar.update msg model.Sidebar
        let newModel, extraCmd =
            match externalMsg with
            | Sidebar.NoOp -> model, Cmd.none
            | Sidebar.LoadSample (fsharpCode, htmlCode, cssCode) ->
                let cmd =
                    match model.State with
                    | Loading -> Cmd.none
                    | _ -> Cmd.ofMsg (StartCompile (Some fsharpCode)) // Trigger a new compilation
                
                { model with 
                    FSharpCode = fsharpCode
                    HtmlCode = htmlCode
                    CssCode = cssCode 
                }
                , cmd

            | Sidebar.Share ->
                model
                , Cmd.OfFunc.either updateQuery (model.FSharpCode, model.HtmlCode, model.CssCode) ShareableUrlReady UpdateQueryFailed

            | Sidebar.Reset ->
                model
                , Router.newUrl Router.Reset

            | Sidebar.ShareToGist ->
                model
                , match subModel.Options.GistToken with
                    | Some token ->
                        Cmd.OfPromise.either postToGist (token, model.FSharpCode, model.HtmlCode, model.CssCode) GistUrlReady ShareGistError
                    | None ->
                        Cmd.ofMsg NoToken

            | Sidebar.StartCompile ->
                model
                , Cmd.ofMsg (StartCompile None)

            | Sidebar.RefreshIframe ->
                model
                , Cmd.ofMsg RefreshIframe

        { newModel with 
            Sidebar = subModel 
        }
        , Cmd.batch [ 
            Cmd.map SidebarMsg cmd
            extraCmd 
        ]

    | ChangeFsharpCode newCode ->
        { model with 
            FSharpCode = newCode 
        }
        , Cmd.none

    | ChangeHtmlCode newCode ->
        { model with 
            HtmlCode = newCode 
        }
        , Cmd.none

    | ChangeCssCode newCode ->
        { model with 
            CssCode = newCode 
        }
        , Cmd.none

    | ShareableUrlReady () ->
        model
        , Toast.message "Copy it from the address bar"
            |> Toast.title "Shareable link is ready"
            |> Toast.position Toast.BottomRight
            |> Toast.icon Fa.Solid.InfoCircle
            |> Toast.timeout (System.TimeSpan.FromSeconds 5.)
            |> Toast.info

    | LoadGistError exn ->
        JS.console.error exn
        model
        , Toast.message "An error occured when loading the gist"
            |> Toast.icon Fa.Solid.ExclamationTriangle
            |> Toast.position Toast.BottomRight
            |> Toast.warning

    | UpdateQueryFailed exn ->
        JS.console.error exn
        model
        , Toast.message "An error occured when updating the URL"
            |> Toast.icon Fa.Solid.ExclamationTriangle
            |> Toast.position Toast.BottomRight
            |> Toast.warning

    | UpdateStats stats ->
        model
        , Cmd.ofMsg (SidebarMsg (Sidebar.UpdateStats stats))

    | RefreshIframe ->
        { model with 
            Logs = [ ConsolePanel.Log.Separator ] 
        }
        , Cmd.OfFunc.perform (Generator.generateHtmlBlobUrl model.HtmlCode model.CssCode) model.CodeES2015 SetIFrameUrl

    | AddConsoleLog content ->
        model
        |> addLog (ConsolePanel.Log.Info content)
        , Cmd.none

    | AddConsoleError content ->
        model
        |> addLog (ConsolePanel.Log.Error content)
        , Cmd.none

    | AddConsoleWarn content ->
        model
        |> addLog (ConsolePanel.Log.Warn content)
        , Cmd.none

let workerCmd (worker : ObservableWorker<_>)=
    let handler dispatch =
        worker
        |> Observable.add (function
            | Loaded ->
                LoadSuccess |> dispatch
            | LoadFailed -> LoadFail |> dispatch
            | ParsedCode errors -> MarkEditorErrors errors |> dispatch
            | CompilationFinished (jsCode, errors, stats) ->
                Ok(jsCode, errors) |> EndCompile |> dispatch
                UpdateStats stats |> dispatch
            | CompilerCrashed msg -> Error msg |> EndCompile |> dispatch
            // Do nothing, these will be handled by .PostAndAwaitResponse
            | FoundTooltip _ -> ()
            | FoundCompletions _ -> ()
            | FoundDeclarationLocation _ -> ()
        )
    [ handler ]

let init () =
    let saved = loadState(Literals.STORAGE_KEY)
    let sidebarModel, sidebarCmd = Sidebar.init ()
    let worker = ObservableWorker(Worker(), WorkerAnswer.Decoder, "MAIN APP")
    CreateChecker(Literals.METADATA_DIR, Literals.EXTRA_REFS,
                  Some ".txt", Some Literals.REPL_LIB_MAP_JSON_URL,
                  sidebarModel.Options.ToOtherFSharpOptions)
    |> worker.Post
    let cmd = Cmd.batch [
                Cmd.ups MouseUp
                Cmd.move MouseMove
                Cmd.iframeMessage
                    { MoveCtor = MouseMove
                      UpCtor = MouseUp
                      ConsoleLogCor = AddConsoleLog
                      ConsoleWarnCor = AddConsoleWarn
                      ConsoleErrorCor = AddConsoleError }
                Cmd.map SidebarMsg sidebarCmd
                workerCmd worker ]

    { 
        State = Loading
        FSharpEditor = Unchecked.defaultof<IEditor>
        Worker = worker
        IFrameUrl = ""
        OutputTab = OutputTab.Live
        CodeTab = CodeTab.FSharp
        CodeES2015 = ""
        FSharpCode = saved.code
        FSharpErrors = [||]
        HtmlCode = saved.html
        CssCode = saved.css
        DragTarget = NoTarget
        PanelSplitRatio = 0.5
        Sidebar = sidebarModel
        IsProblemsPanelExpanded = true
        Logs = [] 
    }, cmd


let private numberToPercent number =
    string (number * 100.) + "%"

let private fontSizeClass =
        function
        | 11. -> "is-small"
        | 14. -> "is-medium"
        | 17. -> "is-large"
        | _ -> "is-medium"

let private htmlEditorOptions (fontSize : float) (fontFamily : string): Monaco.Editor.IEditorConstructionOptions =
    jsOptions(fun o ->
        o.language <- Some "html"
        o.fontSize <- Some fontSize
        o.theme <- Some "vs-dark"
        o.minimap <- Some (jsOptions(fun oMinimap ->
            oMinimap.enabled <- Some false))
        o.fontFamily <- Some fontFamily
        o.fontLigatures <- Some (fontFamily = "Fira Code")
    )

let private cssEditorOptions (fontSize : float) (fontFamily : string) =
    jsOptions<Monaco.Editor.IEditorConstructionOptions>(fun o ->
        let minimapOptions =  jsOptions<Monaco.Editor.IEditorMinimapOptions>(fun oMinimap ->
            oMinimap.enabled <- Some false
        )
        o.language <- Some "css"
        o.fontSize <- Some fontSize
        o.theme <- Some "vs-dark"
        o.minimap <- Some minimapOptions
        o.fontFamily <- Some fontFamily
        o.fontLigatures <- Some (fontFamily = "Fira Code")
    )

let private fsharpEditorOptions (fontSize : float) (fontFamily : string) =
    jsOptions<Monaco.Editor.IEditorConstructionOptions>(fun o ->
        let minimapOptions = jsOptions<Monaco.Editor.IEditorMinimapOptions>(fun oMinimap ->
            oMinimap.enabled <- Some false
        )
        o.language <- Some "fsharp"
        o.fontSize <- Some fontSize
        o.theme <- Some "vs-dark"
        o.minimap <- Some minimapOptions
        o.fontFamily <- Some fontFamily
        o.fontLigatures <- Some (fontFamily = "Fira Code")
        o.fixedOverflowWidgets <- Some true
    )

let private editorTabs (activeTab : CodeTab) dispatch =
    Bulma.tabs [
        tabs.isCentered
        tabs.isMedium
        tabs.isToggle
        prop.children [
            Html.ul [
                Html.li [
                    if (activeTab = CodeTab.FSharp) then
                        prop.className "is-active"
                    prop.onClick (fun _ -> SetCodeTab CodeTab.FSharp |> dispatch)
                    prop.children [
                        Html.a [
                            prop.text "F#"
                        ]
                    ]
                ]

                Html.li [
                    if (activeTab = CodeTab.Html) then
                        prop.className "is-active"
                    prop.onClick (fun _ -> SetCodeTab CodeTab.Html |> dispatch)
                    prop.children [
                        Html.a [
                            prop.text "HTML"
                        ]
                    ]
                ]

                Html.li [
                    if (activeTab = CodeTab.Css) then
                        prop.className "is-active"
                    prop.onClick (fun _ -> SetCodeTab CodeTab.Css |> dispatch)
                    prop.children [
                        Html.a [
                            prop.text "CSS"
                        ]
                    ]
                ]
            ]
        ]
    ]

let private problemsPanel (isExpanded : bool) (errors : Monaco.Editor.IMarkerData[]) (currentTab : CodeTab) dispatch =
    let bodyDisplay =
        if isExpanded then
            ""
        else
            "is-hidden"

    let headerIcon =
        if isExpanded then
            Fa.Solid.AngleDown
        else
            Fa.Solid.AngleUp

    let title =
        if errors.Length = 0 then
            Html.span [ 
                prop.text "Problems" 
            ] 
        else
            Html.span [ 
                prop.children [
                    Html.text  "Problems: "
                    Html.span [
                        prop.style [
                            style.marginLeft (length.em 0.5)
                        ]
                        prop.text (string errors.Length)
                    ]
                ]
           ]

    Html.div [
        prop.className "scrollable-panel is-problem"
        prop.children [
            Html.div [
                prop.className "scrollable-panel-header"
                prop.onClick (fun _ -> dispatch ToggleProblemsPanel)
                prop.children [
                    Html.div [
                        prop.className "scrollable-panel-header-icon"
                        prop.children [
                            Bulma.icon [
                                Fa.i [ headerIcon; Fa.Size Fa.FaLarge ] [ ]
                            ]
                        ]
                    ]

                    Html.div [
                        prop.className "scrollable-panel-header-title"
                        prop.children [ title ]
                    ]

                    Html.div [ 
                        prop.className "scrollable-panel-header-icon"
                        prop.children [ 
                            Bulma.icon [ 
                                Fa.i [ headerIcon; Fa.Size Fa.FaLarge ] [ ] 
                            ] 
                        ]
                    ]
                ]
            ]

            Html.div [
                prop.className ("scrollable-panel-body " + bodyDisplay)
                prop.children [
                    for error in errors do
                        match error.severity with
                        | Monaco.MarkerSeverity.Error
                        | Monaco.MarkerSeverity.Warning ->
                            let (faIcon, colorProperty, colorClass) =
                                match error.severity with
                                | Monaco.MarkerSeverity.Error -> Fa.Solid.TimesCircle, color.isDanger, "is-danger"
                                | Monaco.MarkerSeverity.Warning -> Fa.Solid.ExclamationTriangle, color.isWarning, "is-warning"
                                | _ -> failwith "Should not happen", color.isDanger, ""

                            Html.div [
                                prop.className ("scrollable-panel-body-row " + colorClass)
                                prop.onClick (fun _ ->
                                    if currentTab <> CodeTab.FSharp then
                                        SetCodeTab CodeTab.FSharp |> dispatch
                                    ReactEditor.Dispatch.cursorMove "fsharp_cursor_jump" error
                                )
                                prop.children [
                                    Bulma.icon [
                                        icon.isSmall
                                        colorProperty
                                        prop.children [
                                            Fa.i [ faIcon ] []
                                        ]
                                    ]
                                    
                                    Html.span [
                                        prop.className "scrollable-panel-body-row-description"
                                        prop.text error.message
                                    ]

                                    Html.span [
                                        prop.className "scrollable-panel-body-row-position"
                                        prop.children [
                                            Html.text "("
                                            Html.text (string error.startLineNumber)
                                            Html.text ","
                                            Html.text (string error.startColumn)
                                            Html.text ")"
                                        ]
                                    ]
                                ]
                            ]
                        | _ -> ()
                ]
            ]
        ]
    ]


let private registerCompileCommand dispatch =
    System.Func<_,_,_>(
        fun (editor : Monaco.Editor.IStandaloneCodeEditor) (monacoModule : Monaco.IExports) ->
            printfn "maxime regsiter compile thing"
            let triggerCompile () = StartCompile None |> dispatch
            editor.addCommand(monacoModule.KeyMod.Alt ||| int Monaco.KeyCode.Enter, triggerCompile, "") |> ignore
            editor.addCommand(monacoModule.KeyMod.CtrlCmd ||| int Monaco.KeyCode.KEY_S, triggerCompile, "") |> ignore
    )

let private onFSharpEditorDidMount model dispatch =
    System.Func<_,_,_>(
        fun (editor : Monaco.Editor.IStandaloneCodeEditor) (monacoModule : Monaco.IExports) ->
            printfn "maxime from f# editor"
            if not (isNull editor) then
                dispatch (SetFSharpEditor editor)

                // Because we have access to the monacoModule here,
                // register the different provider needed for F# editor
                let getTooltip line column lineText =
                    async {
                        let id = System.Guid.NewGuid()
                        return! model.Worker.PostAndAwaitResponse(GetTooltip(id, line, column, lineText), function
                            | FoundTooltip(id2, lines) when id = id2 -> Some lines
                            | _ -> None)
                    }

                let tooltipProvider = Editor.createTooltipProvider getTooltip
                monacoModule.languages.registerHoverProvider("fsharp", tooltipProvider) |> ignore

                let getDeclarationLocation uri line column lineText =
                    async {
                        let id = System.Guid.NewGuid()
                        return! model.Worker.PostAndAwaitResponse(GetDeclarationLocation(id, line, column, lineText), function
                            | FoundDeclarationLocation(id2, res) when id = id2 ->
                                res |> Option.map (fun (line1, col1, line2, col2) ->
                                    uri, line1, col1, line2, col2) |> Some
                            | _ -> None)
                    }

                let editorUri = editor.getModel().uri
                let definitionProvider = Editor.createDefinitionProvider (getDeclarationLocation editorUri)
                monacoModule.languages.registerDefinitionProvider("fsharp", definitionProvider) |> ignore

                let getCompletion line column lineText =
                    async {
                        let id = System.Guid.NewGuid()
                        return! model.Worker.PostAndAwaitResponse(GetCompletions(id, line, column, lineText), function
                            | FoundCompletions(id2, lines) when id = id2 -> Some lines
                            | _ -> None)
                    }

                let completionProvider = Editor.createCompletionProvider getCompletion
                monacoModule.languages.registerCompletionItemProvider("fsharp", completionProvider) |> ignore

                (registerCompileCommand dispatch).Invoke(editor, monacoModule)
    )


let private editorArea model dispatch =
    Html.div [
        prop.className "vertical-panel"
        prop.style [
            style.width (length.percent (model.PanelSplitRatio * 100.))
            style.position.relative
        ]
        prop.children [
            editorTabs model.CodeTab dispatch
            // Html editor
            ReactEditor.editor [ 
                editor.options (htmlEditorOptions model.Sidebar.Options.FontSize model.Sidebar.Options.FontFamily)
                editor.value model.HtmlCode
                editor.isHidden (model.CodeTab <> CodeTab.Html)
                editor.customClass (fontSizeClass model.Sidebar.Options.FontSize)
                editor.onChange (ChangeHtmlCode >> dispatch)
                editor.editorDidMount (registerCompileCommand dispatch) 
            ]
            // Css editor
            ReactEditor.editor [ 
                editor.options (cssEditorOptions model.Sidebar.Options.FontSize model.Sidebar.Options.FontFamily)
                editor.value model.CssCode
                editor.isHidden (model.CodeTab <> CodeTab.Css)
                editor.customClass (fontSizeClass model.Sidebar.Options.FontSize)
                editor.onChange (ChangeCssCode >> dispatch)
                editor.editorDidMount (registerCompileCommand dispatch) 
            ]
            // F# editor
            ReactEditor.editor [ 
                editor.options (fsharpEditorOptions model.Sidebar.Options.FontSize model.Sidebar.Options.FontFamily)
                editor.value model.FSharpCode
                editor.isHidden (model.CodeTab <> CodeTab.FSharp)
                editor.onChange (ChangeFsharpCode >> dispatch)
                editor.errors model.FSharpErrors
                editor.eventId "fsharp_cursor_jump"
                editor.customClass (fontSizeClass model.Sidebar.Options.FontSize)
                editor.editorDidMount (onFSharpEditorDidMount model dispatch)
            ]
            problemsPanel model.IsProblemsPanelExpanded model.FSharpErrors model.CodeTab dispatch
        ]
    ]

let private outputTabs (activeTab : OutputTab) dispatch =
    Bulma.tabs [
        tabs.isCentered
        tabs.isMedium
        tabs.isToggle
        prop.children [
            Html.ul [

                Html.li [
                    if (activeTab = OutputTab.Live) then
                        prop.className "is-active"
                    prop.onClick (fun _ ->
                        SetOutputTab OutputTab.Live |> dispatch
                    )
                    prop.children [
                        Html.a [ 
                            prop.text "Live sample"
                        ]
                    ]
                ]

                Html.li [
                    if (activeTab = OutputTab.Code) then
                        prop.className "is-active"
                    prop.onClick (fun _ ->
                        SetOutputTab OutputTab.Code |> dispatch
                    )
                    prop.children [
                        Html.a [ 
                            prop.text "Code"
                        ]
                    ]
                ]
            ]
        ]
    ]

let private toggleDisplay cond =
    if cond then "" else "is-hidden"

let private viewIframe isShown url =
    Html.iframe [ 
        prop.src url
        prop.className (toggleDisplay isShown) 
    ]

let private viewCodeEditor (model: Model) =
    let fontFamily = model.Sidebar.Options.FontFamily
    let options = jsOptions<Monaco.Editor.IEditorConstructionOptions>(fun o ->
                        let minimapOptions = jsOptions<Monaco.Editor.IEditorMinimapOptions>(fun oMinimap ->
                            oMinimap.enabled <- Some false
                        )
                        o.language <- Some "javascript"
                        o.fontSize <- Some model.Sidebar.Options.FontSize
                        o.theme <- Some "vs-dark"
                        o.minimap <- Some minimapOptions
                        o.readOnly <- Some true
                        o.fontFamily <- Some fontFamily
                        o.fontLigatures <- Some (fontFamily = "Fira Code")
                    )

    ReactEditor.editor [ 
        editor.options options
        editor.value model.CodeES2015
        editor.isHidden (model.OutputTab <> OutputTab.Code)
        editor.customClass (fontSizeClass model.Sidebar.Options.FontSize) 
    ]

let private outputArea model dispatch =
    let isLiveViewShown = model.OutputTab = OutputTab.Live
    
    Html.div [ 
        prop.className "output-container"
        prop.style [
            style.width (length.percent ((1. - model.PanelSplitRatio) * 100.))
        ]  
        prop.children [
            outputTabs model.OutputTab dispatch
            
            Html.div [
                prop.className "output-content"
                prop.style [
                    style.height (length.percent 100)
                ]
                prop.children [
                    if model.State = Loading then
                        Html.div [ 
                            prop.className [
                                true, "is-loading title has-text-centered"
                                not isLiveViewShown, "is-hidden"
                            ]
                            prop.style [
                                style.height (length.percent 100)
                                style.fontSize (length.em 1.2)
                            ]
                        ]
                    else
                        viewIframe isLiveViewShown model.IFrameUrl
                    viewCodeEditor model
                    ConsolePanel.consolePanel { Logs = model.Logs }
                ]
            ]
        ]
    ]

let view (model: Model) dispatch =
    Elmish.React.Common.lazyView2
        (fun model dispatch ->
            let isDragging =
                match model.DragTarget with
                | PanelSplitter -> true
                | NoTarget -> false

            Html.div [
                prop.className [
                    isDragging, "is-unselectable"
                ]
                prop.children [
                    Html.div [
                        prop.className "page-content"
                        prop.children [
                            Sidebar.view (model.State = State.Compiling) model.Sidebar (SidebarMsg >> dispatch)

                            Html.div [
                                prop.className "main-content"
                                prop.children [
                                    editorArea model dispatch

                                    Html.div [
                                        prop.className "horizontal-resize"
                                        prop.onMouseDown (fun ev ->
                                            // This prevents text selection in Safari
                                            ev.preventDefault()
                                            dispatch PanelDragStarted
                                        )
                                    ]
                                    
                                    outputArea model dispatch
                                ]
                            ]
                        ]
                    ]
                ]
            ]
        )
        model
        dispatch
