module Main exposing (main)

import Html exposing (Html, text)


main : Program Never State Msg
main =
    Html.program
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


type App
    = App AppName (List Model)


type AppName
    = AppName String


type Model
    = Model ModelName (List Field)


type ModelName
    = ModelName String


type Field
    = Field FieldName DataType


type FieldName
    = FieldName String


type DataType
    = IntType
    | FloatType
    | StringType
    | TextType
    | BoolType
    | DateType
    | DateTimeType
    | ArrayType
    | JsonType


type State
    = State (List App) App


newApp : String -> App
newApp name =
    App (AppName name) []


initialState : State
initialState =
    State [] (newApp "Blog")


init : ( State, Cmd msg )
init =
    initialState ! []


type Msg
    = NoOp
    | Export


update : Msg -> State -> ( State, Cmd Msg )
update msg state =
    case msg of
        NoOp ->
            state ! []

        Export ->
            state ! []


subscriptions : State -> Sub Msg
subscriptions _ =
    Sub.none


view : State -> Html Msg
view state =
    text "hello world"
