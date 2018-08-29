module App
    exposing
        ( App
        , DataType(..)
        , Field
        , Model
        , Name
        , dataType
        , empty
        , fieldName
        , fields
        , modelName
        , models
        , name
        , sampleApp
        )


sampleApp : App
sampleApp =
    App "blog"
        [ Model "user"
            [ Field "email" StringType
            , Field "name" StringType
            , Field "isAdmin" BooleanType
            , Field "aliases" (ArrayType StringType)
            ]
        , Model "post"
            [ Field "title" StringType
            , Field "content" TextType
            , Field "isPublished" BooleanType
            , Field "dateCreated" DateType
            ]
        ]



-- Types


type App
    = App Name (List Model)


type Model
    = Model Name (List Field)


type Field
    = Field Name DataType


type alias Name =
    String


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



-- Build


empty : App
empty =
    App "" []



-- Read


name : App -> Name
name (App name_ _) =
    name_


models : App -> List Model
models (App _ models_) =
    models_


modelName : Model -> Name
modelName (Model name_ _) =
    name_


fields : Model -> List Field
fields (Model _ fields_) =
    fields_


fieldName : Field -> Name
fieldName (Field name_ _) =
    name_


dataType : Field -> DataType
dataType (Field name_ dataType_) =
    dataType_
