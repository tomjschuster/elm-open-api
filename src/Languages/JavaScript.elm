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
        , require
        , return
        , set
        , string
        , useStrict
        , var
        )

import Doc
    exposing
        ( (|+)
        , Doc
        , braces
        , brackets
        , char
        , join
        , parens
        , space
        )
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


require : String -> Doc
require path =
    Doc.string "require"
        |+ parens (string path)


moduleExports : Doc -> Doc
moduleExports expression =
    set "module.exports" expression


declare : String -> String -> Doc -> Doc
declare label variable expression =
    Doc.string label
        |+ space
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
        |+ space
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
        |+ space
        |+ expression
        |+ semicolon


args : List String -> Doc
args argList =
    argList
        |> List.map Doc.string
        |> join comma
        |> parens



-- Control Flow


block : Doc -> Doc
block body =
    braces body



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
        |> join comma
        |> braces


array : List Doc -> Doc
array expressions =
    expressions
        |> join comma
        |> brackets


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



-- Character Helpers


fatArrow : Doc
fatArrow =
    Doc.string "=>"


comma : Doc
comma =
    char ','


equals : Doc
equals =
    char '='


colon : Doc
colon =
    char ':'


semicolon : Doc
semicolon =
    char ';'
