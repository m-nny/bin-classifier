module Utils exposing (fill, set)
import List exposing (take, drop, concat)

fill: value -> List a -> List value
fill value l =
    List.map (always value) l

set: Int -> value -> List value -> List value
set index value l =
    let 
        h = take (index) l
        t = drop (index + 1) l
    in
        concat [h, [value], t]