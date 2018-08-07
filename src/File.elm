module File exposing (File, content, directory, encode, file, name, root)

import Json.Encode as JE


type File
    = File Name Content
    | Directory Name (List File)
    | Root (List File)


type Name
    = Name String


type Content
    = Content String


file : Name -> Content -> File
file name content =
    File name content


directory : Name -> List File -> File
directory name files =
    Directory name files


root : List File -> File
root files =
    Root files


name : String -> Name
name value =
    Name value


content : String -> Content
content value =
    Content value


encode : File -> JE.Value
encode file =
    case file of
        File name content ->
            JE.object
                [ ( "type", JE.string "file" )
                , ( "name", encodeName name )
                , ( "content", encodeContent content )
                ]

        Directory name files ->
            JE.object
                [ ( "type", JE.string "directory" )
                , ( "name", encodeName name )
                , ( "files", JE.list (List.map encode files) )
                ]

        Root files ->
            JE.object
                [ ( "type", JE.string "root" )
                , ( "files", JE.list (List.map encode files) )
                ]


encodeName : Name -> JE.Value
encodeName (Name name) =
    JE.string name


encodeContent : Content -> JE.Value
encodeContent (Content content) =
    JE.string content
