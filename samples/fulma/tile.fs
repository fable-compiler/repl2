// More info about Fulma at https://mangelmaxime.github.io/Fulma/
module Fulma.Tile

open Fable.React
open Fable.React.Props
open Fulma

let iconInteractive () =
    Tile.ancestor [ ]
        [ Tile.parent [ Tile.IsVertical
                        Tile.Size Tile.Is4 ]
            [ Tile.child [ ]
                [ Box.box' [ ]
                    [ Heading.p [ ]
                        [ str "Tile n°1" ]
                      p [ ]
                        [ str "Nulla at urna iaculis, eleifend dolor eget, pellentesque eros. Mauris luctus pharetra velit, viverra feugiat nibh \
                                vehicula vitae. Suspendisse vitae sem id." ] ] ]
              Tile.child [ ]
                [ Box.box' [ ]
                    [ Heading.p [ ]
                        [ str "Tile n°2" ]
                      p [ ]
                        [ str "Curabitur pretium nisi tortor, vitae elementum justo blandit sit amet. Pellentesque vel commodo metus. \
                                In scelerisque pretium quam, quis varius lectus maximus sed." ] ] ]
            ]
          Tile.parent [ ]
            [ Tile.child [ ]
                [ Box.box' [ Common.Props [ Style [ Height "100%" ] ] ]
                    [ Heading.p [ ]
                        [ str "Tile n°3" ]
                      p [ ]
                        [ str "Etiam quis neque efficitur, iaculis urna eget, efficitur ligula. Cras faucibus, magna eu eleifend maximus, ligula \
                                ex gravida libero, vitae suscipit velit nibh eget eros." ]
                      p [ ]
                        [ str "Suspendisse vel turpis nisi. Fusce at risus accumsan, varius massa id, dictum est. Aenean consequat neque \
                                sed tincidunt eleifend." ]
                      p [ ]
                        [ str "Phasellus ac lectus in ex condimentum sollicitudin. Sed id mollis turpis. Sed at \
                                felis vel diam interdum viverra." ] ] ] ] ]

div [] [
    Card.card [] [Card.content [] [iconInteractive()] ]
] |> mountById "elmish-app"
