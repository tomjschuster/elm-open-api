module File exposing (File, add, addToList, content, directory, empty, encode, file, name)

import Json.Encode as JE


type File
    = File Name Content
    | Directory Name (List File)


type alias Name =
    String


type Content
    = Content String


type alias Path =
    List Name


type Error
    = NotFound
    | AlreadyExists
    | MultipleFound
    | AddFileToFile


empty : Name -> File
empty fileName =
    File fileName emptyContent


name : File -> Name
name file =
    case file of
        File name _ ->
            name

        Directory name _ ->
            name


add : File -> Path -> File -> Result Error File
add newFile path file =
    case file of
        File _ _ ->
            Err AddFileToFile

        Directory dirName files ->
            case shiftPath path of
                ( Just pathName, remainingPath ) ->
                    if pathName == dirName then
                        files
                            |> addToList newFile remainingPath
                            |> Result.map (Directory dirName)
                    else
                        Err NotFound

                ( Nothing, remainingPath ) ->
                    Err NotFound


addToList : File -> Path -> List File -> Result Error (List File)
addToList file path files =
    if List.any (nameMatches (name file)) files then
        Err AlreadyExists
    else
        case shiftPath path of
            ( Nothing, _ ) ->
                Ok (files ++ [ file ])

            ( Just ".", [] ) ->
                Ok (files ++ [ file ])

            ( Just pathName, remainingPath ) ->
                let
                    ( result, found ) =
                        List.foldr
                            (\currFile ( files, added ) ->
                                case ( nameMatches pathName currFile, added ) of
                                    ( True, True ) ->
                                        ( Err MultipleFound, True )

                                    ( True, False ) ->
                                        ( Result.map2 (::) (add file remainingPath currFile) files, True )

                                    ( False, _ ) ->
                                        ( Result.map (\xs -> currFile :: xs) files, added )
                            )
                            ( Ok [], False )
                            files
                in
                if found then
                    result
                else
                    Err NotFound


nameMatches : Name -> File -> Bool
nameMatches nameString file =
    nameString == name file


shiftPath : Path -> ( Maybe String, Path )
shiftPath path =
    case path of
        [] ->
            ( Nothing, path )

        x :: xs ->
            ( Just x, xs )


file : Name -> Content -> File
file name content =
    File name content


directory : Name -> List File -> File
directory name files =
    Directory name files


emptyContent : Content
emptyContent =
    Content ""


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
encodeName name =
    JE.string name


encodeContent : Content -> JE.Value
encodeContent (Content content) =
    JE.string content
