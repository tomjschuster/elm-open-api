module Languages.JavaScript
    exposing
        ( array
        , arrowFunction
        , block
        , blockComment
        , conciseArrowFunction
        , const
        , function
        , functionCall
        , let_
        , methodCall
        , moduleExports
        , namedFunction
        , object
        , property
        , require
        , return
        , set
        , singleComment
        , string
        , useStrict
        , var
        , variable
        )

import Doc exposing ((|+), Doc)
import Regex exposing (Regex)
import String.Extra


{- Statements -}
-- Declarations


var : String -> Doc -> Doc
var variable expression =
    declare "var" variable expression


let_ : String -> Doc -> Doc
let_ variable expression =
    declare "let" variable expression


const : String -> Doc -> Doc
const variable expression =
    declare "const" variable expression


variable : String -> Doc
variable variable =
    Doc.string variable


set : Doc -> Doc -> Doc
set expression variable =
    variable
        |+ equals
        |+ expression
        |+ semicolon


moduleExports : Doc -> Doc
moduleExports expression =
    variable "module"
        |> property "exports"
        |> set expression


declare : String -> String -> Doc -> Doc
declare label variable expression =
    Doc.string label
        |+ Doc.space
        |+ Doc.string variable
        |+ equals
        |+ expression
        |+ semicolon



-- Functions


function : List String -> List Doc -> Doc
function argList statements =
    Doc.string "function"
        |+ declareArgs argList
        |+ block statements


namedFunction : String -> List String -> List Doc -> Doc
namedFunction name argList statements =
    Doc.string "function"
        |+ Doc.space
        |+ Doc.string name
        |+ declareArgs argList
        |+ block statements


arrowFunction : List String -> List Doc -> Doc
arrowFunction argList statements =
    declareArgs argList
        |+ fatArrow
        |+ block statements


conciseArrowFunction : List String -> Doc -> Doc
conciseArrowFunction argList statement =
    declareArgs argList
        |+ fatArrow
        |+ statement


return : Doc -> Doc
return expression =
    Doc.string "return"
        |+ Doc.space
        |+ expression
        |+ semicolon


functionCall : String -> List Doc -> Doc
functionCall name callArgs =
    Doc.string name
        |+ Doc.parens (Doc.join comma callArgs)


methodCall : String -> List Doc -> Doc -> Doc
methodCall name callArgs object =
    object
        |+ dot
        |+ functionCall name callArgs


declareArgs : List String -> Doc
declareArgs argList =
    argList
        |> List.map Doc.string
        |> Doc.join comma
        |> Doc.parens



-- Control Flow


block : List Doc -> Doc
block statements =
    Doc.braces (Doc.join semicolon statements)



-- Other


require : String -> Doc
require path =
    Doc.string "require"
        |+ Doc.parens (string path)



{- Expressions -}
-- Data Types


string : String -> Doc
string value =
    value
        |> String.Extra.replace "'" "\\'"
        |> Doc.string
        |> Doc.squotes


object : List ( String, Doc ) -> Doc
object keyValues =
    keyValues
        |> List.map (uncurry keyValue)
        |> Doc.join comma
        |> Doc.braces


property : String -> Doc -> Doc
property name object =
    if onlyWordChars name then
        object |+ dot |+ Doc.string name
    else
        object |+ Doc.brackets (string name)


onlyWordChars : String -> Bool
onlyWordChars value =
    Regex.contains nonASCIIRegex value


nonASCIIRegex : Regex
nonASCIIRegex =
    Regex.regex "^[_$a-zA-Z\\xA0-\\uFFFF][_$a-zA-Z0-9 -\\uFFFF]*$"


array : List Doc -> Doc
array expressions =
    expressions
        |> Doc.join comma
        |> Doc.brackets


keyValue : String -> Doc -> Doc
keyValue key expression =
    string key
        |+ colon
        |+ expression



-- Other


useStrict : Doc
useStrict =
    string "use strict"
        |+ semicolon



{- Grammar -}


singleComment : String -> Doc
singleComment comment =
    Doc.line
        |+ slash
        |+ slash
        |+ Doc.space
        |+ Doc.string comment
        |+ Doc.line


blockComment : String -> Doc
blockComment comment =
    Doc.surround (Doc.string "/*") (Doc.string "*/") (Doc.string comment)


fatArrow : Doc
fatArrow =
    Doc.string "=>"


comma : Doc
comma =
    Doc.char ','


equals : Doc
equals =
    Doc.char '='


colon : Doc
colon =
    Doc.char ':'


semicolon : Doc
semicolon =
    Doc.char ';'


dot : Doc
dot =
    Doc.char '.'


slash : Doc
slash =
    Doc.char '/'
