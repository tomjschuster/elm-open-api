module File exposing (File, content, directory, encode, file, name)

import Json.Encode as JE


type File
    = File Name Content
    | Directory Name (List File)


type Name
    = Name String


type Content
    = Content String


type Path
    = Path (List String)


add : File -> Path -> File -> Result String File
add newFile path file =
    case file of
        File _ _ ->
            Err "Files can only be added to root or directory"

        Directory name files ->
            case shiftPath path of
                ( Nothing, _ ) ->
                    Ok (Directory name (files ++ [ file ]))

                ( Just ".", Path [] ) ->
                    Ok (Directory name (files ++ [ file ]))

                ( Just name, files ) ->
                    Err "do something"


nameMatches : String -> Name -> Bool
nameMatches nameString (Name matchName) =
    nameString == matchName


pathLength : Path -> Int
pathLength (Path path) =
    List.length path


shiftPath : Path -> ( Maybe String, Path )
shiftPath (Path path) =
    case path of
        [] ->
            ( Nothing, Path path )

        x :: xs ->
            ( Just x, Path xs )


file : Name -> Content -> File
file name content =
    File name content


directory : Name -> List File -> File
directory name files =
    Directory name files


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


encodeName : Name -> JE.Value
encodeName (Name name) =
    JE.string name


encodeContent : Content -> JE.Value
encodeContent (Content content) =
    JE.string content
