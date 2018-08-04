module QuickApp exposing (main)

import Html exposing (Html, text)


main =
    Html.program
        { init = ( (), Cmd.none )
        , update = always (always ( (), Cmd.none ))
        , subscriptions = always Sub.none
        , view = always (text "Hello World")
        }
