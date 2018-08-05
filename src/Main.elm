port module Main exposing (main)

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


type DirectoryChild
    = FileChild File
    | DirectoryChild Directory


type File
    = File FileName FileContent


type FileName
    = FileName String


type FileContent
    = FileContent String


type Directory
    = Directory DirectoryName (List DirectoryChild)


type DirectoryName
    = DirectoryName String


encodeDirectory : Directory -> JE.Value
encodeDirectory (Directory name children) =
    JE.object [ ( "type", JE.string "directory" ), ( "name", encodeDirectoryName name ), ( "children", JE.list (List.map encodeDirectoryChild children) ) ]


encodeDirectoryName : DirectoryName -> JE.Value
encodeDirectoryName (DirectoryName name) =
    JE.string name


encodeFile : File -> JE.Value
encodeFile (File fileName fileContent) =
    JE.object [ ( "type", JE.string "file" ), ( "name", encodeFileName fileName ), ( "content", encodeFileContent fileContent ) ]


encodeFileName : FileName -> JE.Value
encodeFileName (FileName name) =
    JE.string name


encodeFileContent : FileContent -> JE.Value
encodeFileContent (FileContent content) =
    JE.string content


encodeDirectoryChild : DirectoryChild -> JE.Value
encodeDirectoryChild directoryChild =
    case directoryChild of
        FileChild file ->
            encodeFile file

        DirectoryChild directory ->
            encodeDirectory directory


appToDirectory : App -> Directory
appToDirectory app =
    Directory (DirectoryName "test-app")
        [ DirectoryChild
            (Directory (DirectoryName "models")
                [ FileChild (File (FileName "user.js") (FileContent "this is the file content"))
                ]
            )
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
            ( state, app |> appToDirectory |> encodeDirectory |> export )


port export : JE.Value -> Cmd msg


subscriptions : State -> Sub Msg
subscriptions _ =
    Sub.none


view : State -> Html Msg
view state =
    button [ Events.onClick Export ] [ text "Export" ]
