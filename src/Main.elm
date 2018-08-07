port module Main exposing (main)

import File exposing (File)
import Html exposing (Html, button, text)
import Html.Events as Events
import Json.Encode as JE


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


testFile : File
testFile =
    File.root
        [ File.directory (File.name "models")
            [ File.file (File.name "user.js")
                (File.content "module.exports = { user: { name: 'Tom' } }")
            ]
        , File.file (File.name "index.js")
            (File.content "const user = require('./models/user.js')")
        ]


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
update msg ((State apps app) as state) =
    case msg of
        NoOp ->
            state ! []

        Export ->
            ( state, zipFile testFile )


port zip : ( String, JE.Value ) -> Cmd msg


zipFile : File -> Cmd msg
zipFile =
    File.encode >> (,) "test-app" >> zip


subscriptions : State -> Sub Msg
subscriptions _ =
    Sub.none


view : State -> Html Msg
view state =
    button [ Events.onClick Export ] [ text "Export" ]
