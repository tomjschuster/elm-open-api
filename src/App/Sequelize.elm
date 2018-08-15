module App.Sequelize exposing (toFiles)

import App exposing (App)
import Doc exposing ((|+), Doc)
import File exposing (File)
import Languages.JavaScript as JS
import String.Extra


type DataType
    = StringType
    | TextType
    | IntegerType
    | FloatType
    | DateTimeType
    | DateType
    | BooleanType
    | ArrayType DataType
    | JsonType
    | BlobType
    | UuidType


mapDataType : App.DataType -> DataType
mapDataType dataType =
    case dataType of
        App.StringType ->
            StringType

        App.TextType ->
            TextType

        App.IntegerType ->
            IntegerType

        App.FloatType ->
            FloatType

        App.DateTimeType ->
            DateTimeType

        App.DateType ->
            DateType

        App.BooleanType ->
            BooleanType

        App.ArrayType arrayType ->
            ArrayType (mapDataType arrayType)

        App.JsonType ->
            JsonType

        App.BlobType ->
            BlobType

        App.UuidType ->
            UuidType


dataTypeConstant : DataType -> String
dataTypeConstant dataType =
    case dataType of
        StringType ->
            "DataTypes.STRING"

        TextType ->
            "DataTypes.TEXT"

        IntegerType ->
            "DataTypes.INTEGER"

        FloatType ->
            "DataTypes.FLOAT"

        DateTimeType ->
            "DataTypes.DATETIME"

        DateType ->
            "DataTypes.DATE"

        BooleanType ->
            "DataTypes.BOOLEAN"

        ArrayType arrayType ->
            "DataTypes.ARRAY(" ++ dataTypeConstant arrayType ++ ")"

        JsonType ->
            "DataTypes.JSON"

        BlobType ->
            "DataTypes.BLOB"

        UuidType ->
            "DataTypes.UUID"


toFiles : App -> List File
toFiles app =
    [ File.directory "models"
        (File.file "index.js" indexContent :: List.map modelFile (App.models app))
    ]


indexContent : String
indexContent =
    """'use strict';

const fs = require('fs');
const path = require('path');
const Sequelize = require('sequelize');
const basename = path.basename(__filename);
const env = process.env.NODE_ENV || 'development';
const config = require('/../config/config.json')[env];
const db = {};

let sequelize;
if (config.use_env_variable) {
  sequelize = new Sequelize(process.env[config.use_env_variable], config);
} else {
  sequelize = new Sequelize(config.database, config.username, config.password, config);
}

fs
  .readdirSync(__dirname)
  .filter(file => {
    return (file.indexOf('.') !== 0) && (file !== basename) && (file.slice(-3) === '.js');
  })
  .forEach(file => {
    const model = sequelize['import'](path.join(__dirname, file));
    db[model.name] = model;
  });

Object.keys(db).forEach(modelName => {
  if (db[modelName].associate) {
    db[modelName].associate(db);
  }
});

db.sequelize = sequelize;
db.Sequelize = Sequelize;

module.exports = db;
    """


modelFile : App.Model -> File
modelFile model =
    File.file (modelFileName model) (modelContent model)


modelFileName : App.Model -> File.Name
modelFileName model =
    String.Extra.classify (App.modelName model) ++ ".js"


modelContent : App.Model -> String
modelContent model =
    Doc.toString <|
        Doc.append JS.useStrict <|
            JS.moduleExports <|
                JS.arrowFunction [ "sequelize", "DataTypes" ] <|
                    JS.const (App.modelName model) <|
                        Doc.string "sequelize"
                            |+ defineModel model


defineModel : App.Model -> Doc
defineModel model =
    Doc.string "sequelize"
        |+ JS.methodCall "define"
            [ JS.string (App.modelName model)
            , JS.object (List.map attributeKV (App.fields model))
            ]


attributeKV : App.Field -> ( String, Doc )
attributeKV field =
    ( App.fieldName field
    , field |> App.dataType |> mapDataType |> dataTypeConstant |> Doc.string
    )
