module Fable.Repl.Mouse

open Fable.Core.JsInterop
open Browser
open Browser.Types
open Thoth.Json

type Position =
    { 
        X : float
        Y : float 
    }

[<RequireQualifiedAccess>]
module Cmd =

    let ups messageCtor =
        let handler dispatch =
            window.addEventListener("mouseup", fun _ ->
                dispatch messageCtor)
        [ handler ]

    let move messageCtor =
        let handler dispatch =
            window.addEventListener("mousemove", fun ev ->
                let ev = ev :?> MouseEvent
                { X = ev.pageX
                  Y = ev.pageY }
                |> messageCtor
                |> dispatch)
        [ handler ]

    type IframeMessageArgs<'Msg> =
        { MoveCtor : Position -> 'Msg
          UpCtor : 'Msg
          ConsoleLogCor : string -> 'Msg
          ConsoleWarnCor : string -> 'Msg
          ConsoleErrorCor : string -> 'Msg }

    let iframeMessage (args : IframeMessageArgs<'Msg>) =
        let handler dispatch =
            window.addEventListener("message", fun ev ->
                let iframeMessageDecoder =
                    Decode.field "type" Decode.string
                    |> Decode.option
                    |> Decode.andThen
                        (function
                        | Some "mousemove" ->
                            Decode.object (fun get ->
                                { X = get.Required.Field "x" Decode.float
                                  Y = get.Required.Field "y" Decode.float })
                            |> Decode.map args.MoveCtor
                        | Some "mouseup" ->
                            Decode.succeed args.UpCtor
                        | Some "console_log" ->
                            Decode.field "content" Decode.string
                            |> Decode.map args.ConsoleLogCor
                        | Some "console_warn" ->
                            Decode.field "content" Decode.string
                            |> Decode.map args.ConsoleWarnCor
                        | Some "console_error" ->
                            Decode.field "content" Decode.string
                            |> Decode.map args.ConsoleErrorCor
                        | x ->
                            // Discard messages we don't know how to handle it
                            sprintf "`%A` is not a known value for an iframe message" x
                            |> Decode.fail
                    )
                Decode.fromValue "$" iframeMessageDecoder ev?data
                |> function
                    | Ok msg -> dispatch msg
                    | Error error -> 
                        // We don't have an easy way to discard false positive
                        // so for now, only log when in DEBUG mode
                        #if DEBUG
                        console.warn error
                        #endif
                        ()
            )
        [ handler ]
