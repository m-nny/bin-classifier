module Data exposing (..)

import File exposing (File)
import Json.Decode exposing (..)
import Json.Decode.Extra exposing (..)
import Json.Decode.Pipeline exposing (hardcoded, optional, required)


type alias Dataset =
    { entries : List Entry, title : String }


emptyDataset : Dataset
emptyDataset =
    Dataset [] "title"


fakeDataset : Dataset
fakeDataset =
    Dataset [ newEntry "m-nny@" 0, newEntry "m-nny@yandex.ru" 1 ] "It it email?"


type alias Entry =
    { id : Int, query : String, mark : Mark }


type alias Mark =
    Maybe Bool


newEntry : String -> Int -> Entry
newEntry query id =
    Entry id query Nothing


datasetDecoder : Decoder Dataset
datasetDecoder =
    succeed Dataset
        |> required "entries" (indexedList entryDecoder)
        |> required "title" string


entryDecoder : Int -> Decoder Entry
entryDecoder id =
    succeed Entry
        |> optional "id" int id
        |> required "query" string
        |> optional "result" (map Just bool) Nothing


decodeDataset : String -> Result Error Dataset
decodeDataset raw =
    decodeString datasetDecoder raw
