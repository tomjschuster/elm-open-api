module App.Sequelize exposing (toFiles)

import App exposing (App)
import Doc exposing ((|+), Doc)
import File exposing (File)
import Languages.JavaScript as JS
import String.Extra


toFiles : App -> List File
toFiles app =
    [ File.directory "server"
        [ File.directory "db"
            [ File.directory "models" (List.map modelFile <| App.models app)
            , File.file "index.js" (indexContent app)
            ]
        ]
    ]


indexContent : App -> String
indexContent app =
    app
        |> App.models
        |> List.map (App.modelName >> importModel)
        |> Doc.concat
        |> Doc.toString


importModel : String -> Doc
importModel modelName =
    JS.const modelName (JS.require ("./models/" ++ modelName ++ ".js"))


modelFile : App.Model -> File
modelFile model =
    File.file (modelFileName <| App.modelName model) (modelContent model)


modelFileName : App.Name -> File.Name
modelFileName name =
    String.Extra.classify name ++ ".js"


modelContent : App.Model -> File.Content
modelContent model =
    ""
