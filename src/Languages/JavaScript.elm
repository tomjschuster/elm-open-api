module Languages.JavaScript
    exposing
        ( array
        , arrowFunction
        , block
        , conciseArrowFunction
        , const
        , function
        , let_
        , moduleExports
        , namedFunction
        , object
        , property
        , require
        , return
        , set
        , string
        , useStrict
        , var
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


set : String -> Doc -> Doc
set objectKey expression =
    Doc.string objectKey
        |+ equals
        |+ expression
        |+ semicolon


moduleExports : Doc -> Doc
moduleExports expression =
    set "module.exports" expression


declare : String -> String -> Doc -> Doc
declare label variable expression =
    Doc.string label
        |+ Doc.space
        |+ Doc.string variable
        |+ equals
        |+ expression
        |+ semicolon



-- Functions


function : List String -> Doc -> Doc
function argList body =
    Doc.string "function"
        |+ args argList
        |+ block body


namedFunction : String -> List String -> Doc -> Doc
namedFunction name argList body =
    Doc.string "function"
        |+ Doc.space
        |+ Doc.string name
        |+ args argList
        |+ block body


arrowFunction : List String -> Doc -> Doc
arrowFunction argList body =
    args argList
        |+ fatArrow
        |+ block body


conciseArrowFunction : List String -> Doc -> Doc
conciseArrowFunction argList body =
    args argList
        |+ fatArrow
        |+ body


return : Doc -> Doc
return expression =
    Doc.string "return"
        |+ Doc.space
        |+ expression
        |+ semicolon


args : List String -> Doc
args argList =
    argList
        |> List.map Doc.string
        |> Doc.join comma
        |> Doc.parens



-- Control Flow


block : Doc -> Doc
block body =
    Doc.braces body



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
