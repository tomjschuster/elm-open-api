module App exposing (App, DataType, Field, Model, empty)


type App
    = App Name (List Model)


type Model
    = Model Name (List Field)


type Field
    = Field Name DataType


type Name
    = Name String


emptyName : Name
emptyName =
    Name ""


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
    App emptyName []
