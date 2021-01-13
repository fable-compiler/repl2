namespace Elmish.React

[<RequireQualifiedAccess>]
module Program =
    open Browser
    open Fable.React

    /// Setup rendering of root React component inside html element identified by placeholderId
    let withReactSynchronous placeholderId (program:Elmish.Program<_,_,_,_>) =
        let setState model dispatch =
            ReactDom.render(
                program.view model dispatch,
                document.getElementById(placeholderId)
            )

        { program with setState = setState }

    let withReact placeholderId (program:Elmish.Program<_,_,_,_>) =
        withReactSynchronous placeholderId program
