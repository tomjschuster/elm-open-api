module App exposing (App, AppType(..), DataType, Field, Model, empty, toFiles)

import File exposing (File)


type App
    = App Name (List Model)


type Model
    = Model Name (List Field)


type Field
    = Field Name DataType


type alias Name =
    String


type AppType
    = Sequelize


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


empty : App
empty =
    App "" []


toFiles : AppType -> App -> List File
toFiles appType app =
    case appType of
        Sequelize ->
            [ File.directory "server"
                [ File.directory "db"
                    [ File.directory "models"
                        [ File.file "user.js" "module.exports = { user: { name: 'Tom' } }" ]
                    , File.file "index.js" "const user = require('./models/user.js')"
                    ]
                ]
            ]
