port module Main exposing (main)

import App exposing (App)
import File exposing (File)
import Html exposing (Html, button, text)
import Html.Events as Events
import Json.Encode as JE


-- Program


main : Program Never State Msg
main =
    Html.program
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- State


type State
    = State (List App) App


initialState : State
initialState =
    State [] App.empty


init : ( State, Cmd msg )
init =
    initialState ! []



-- Ports


port zip : ( String, JE.Value ) -> Cmd msg



-- Update


type Msg
    = NoOp
    | Export


update : Msg -> State -> ( State, Cmd Msg )
update msg ((State apps app) as state) =
    case msg of
        NoOp ->
            state ! []

        Export ->
            ( state, app |> App.toFiles App.Sequelize |> zipFiles )


zipFiles : List File -> Cmd msg
zipFiles =
    List.map File.encode >> JE.list >> (,) "test-app" >> zip



-- Subscriptions


subscriptions : State -> Sub Msg
subscriptions _ =
    Sub.none



-- View


view : State -> Html Msg
view state =
    button [ Events.onClick Export ] [ text "Export" ]
