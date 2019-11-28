module Data exposing (..)
import Json.Encode exposing (..)
import Json.Encode.Extra exposing (..)
import Json.Decode exposing (field, oneOf)
import Utils

import Debug

type alias Dataset =
    { description : String
    , entries : List String
    , marks : List (Maybe Bool)
    }

datasetMarkEntry: Int -> Bool -> Dataset -> Dataset
datasetMarkEntry index value dataset =
    let newMarks = Utils.set index (Just value) dataset.marks in
    { dataset | marks = newMarks }

decodeDataset : Json.Decode.Decoder Dataset
decodeDataset =
    Json.Decode.map3 Dataset
        (field "description" Json.Decode.string)
        decodeDatasetEntries
        decodeDatasetMarks

decodeDatasetEntries: Json.Decode.Decoder (List String)
decodeDatasetEntries =
    (field "entries" (Json.Decode.list Json.Decode.string))

decodeDatasetMarks: Json.Decode.Decoder (List (Maybe Bool))
decodeDatasetMarks =
        (oneOf
            [ (field "marks" (Json.Decode.list (Json.Decode.map Just Json.Decode.bool)))
            , (Json.Decode.map (Utils.fill Nothing) decodeDatasetEntries)
            ])

encodeDataset : Dataset -> Json.Encode.Value
encodeDataset record =
    Json.Encode.object
        [ ("entries",  Json.Encode.list Json.Encode.string record.entries)
        , ("description",  Json.Encode.string  record.description)
        , ("marks",  Json.Encode.list (maybe Json.Encode.bool) record.marks)
        ]

