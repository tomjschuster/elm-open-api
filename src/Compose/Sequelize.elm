module Compose.Sequelize exposing (toFiles)

import App exposing (App)
import Doc exposing ((|+), Doc)
import File exposing (File)
import Languages.JavaScript as JS
import String.Extra


toFiles : App -> List File
toFiles app =
    [ File.directory "config" []
    , File.directory "migrations" []
    , File.directory "models" <|
        File.file "index.js" indexContent
            :: List.map modelFile (App.models app)
    , File.directory "seeders" []
    ]



-- Data Types


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


dataTypeConstant : DataType -> Doc
dataTypeConstant dataType =
    case dataType of
        StringType ->
            JS.variable "DataTypes"
                |> JS.property "STRING"

        TextType ->
            JS.variable "DataTypes"
                |> JS.property "TEXT"

        IntegerType ->
            JS.variable "DataTypes"
                |> JS.property "INTEGER"

        FloatType ->
            JS.variable "DataTypes"
                |> JS.property "FLOAT"

        DateTimeType ->
            JS.variable "DataTypes"
                |> JS.property "DATETIME"

        DateType ->
            JS.variable "DataTypes"
                |> JS.property "DATE"

        BooleanType ->
            JS.variable "DataTypes"
                |> JS.property "BOOLEAN"

        ArrayType arrayType ->
            JS.variable "DataTypes"
                |> JS.methodCall "ARRAY" [ dataTypeConstant arrayType ]

        JsonType ->
            JS.variable "DataTypes"
                |> JS.property "JSON"

        BlobType ->
            JS.variable "DataTypes"
                |> JS.property "BLOB"

        UuidType ->
            JS.variable "DataTypes"
                |> JS.property "UUID"



-- Models


indexContent : String
indexContent =
    """'use strict';

const fs = require('fs');
const path = require('path');
const Sequelize = require('sequelize');
const basename = path.basename(__filename);
const env = process.env.NODE_ENV || 'development';
const config = require(_dirname + '/../config/config.json')[env];
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
    File.file
        (modelFileName model)
        (Doc.toString <| Doc.concat <| modelContent model)


modelFileName : App.Model -> File.Name
modelFileName model =
    String.Extra.dasherize (App.modelName model) ++ ".js"


modelContent : App.Model -> List Doc
modelContent model =
    let
        modelName =
            model |> App.modelName |> String.Extra.classify

        fields =
            App.fields model
    in
    [ JS.useStrict
    , JS.moduleExports <|
        JS.arrowFunction [ "sequelize", "DataTypes" ]
            [ declareModel modelName fields
            , setAssociations modelName
            , JS.return (JS.variable modelName)
            ]
    ]


declareModel : String -> List App.Field -> Doc
declareModel modelName fields =
    JS.variable "sequelize"
        |> JS.methodCall "define"
            [ JS.string modelName
            , JS.object (List.map attributeKV fields)
            , JS.object []
            ]
        |> JS.const modelName


setAssociations : String -> Doc
setAssociations modelName =
    JS.variable modelName
        |> JS.property "associate"
        |> JS.set (JS.function [ "models" ] [ associationsComment ])


associationsComment : Doc
associationsComment =
    JS.singleComment "associations can be defined here"


attributeKV : App.Field -> ( String, Doc )
attributeKV field =
    ( App.fieldName field
    , field |> App.dataType |> mapDataType |> dataTypeConstant
    )
