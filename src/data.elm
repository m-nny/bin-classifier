module Data exposing (..)


type alias Dataset =
    List Entry


type alias Entry =
    { query : String, id : Int, mark : Mark }


type alias Mark =
    Maybe Bool


newEntry : String -> Int -> Entry
newEntry query id =
    Entry query id Nothing
