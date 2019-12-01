module Lego exposing (..)

import Data exposing (..)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (..)
import Element.Font as Font
import Element.Input exposing (button)


rootAttributes : Element msg -> List (Attribute msg)
rootAttributes lefter =
    [ width (fill |> maximum 600)
    , height fill
    , centerX
    , spacing (size Normal)
    , padding (size Normal)
    , Font.family
        [ Font.external
            { name = "Roboto"
            , url = "https://fonts.googleapis.com/css?family=Roboto:300,300i"
            }
        , Font.sansSerif
        ]
    , Font.light
    , Font.size (size Normal)
    , onLeft lefter
    ]


card : List (Attribute msg) -> List (Attribute msg)
card =
    List.append
        [ centerX
        , width fill
        , Border.shadow { blur = 4, color = rgba 0 0 0 0.25, offset = ( 0, 4 ), size = 0 }
        , Background.color (rgb255 255 255 255)
        , Border.rounded 10
        ]


centeredText : String -> Element msg
centeredText t =
    el [ centerX, centerY ] (text t)


type MarkType
    = Positive
    | Negative


markAlphaValue : MarkType -> Mark -> ( Float, String )
markAlphaValue markType mark =
    ( case mark of
        Nothing ->
            0.3

        Just isPositive ->
            if (markType == Positive && isPositive) || (markType == Negative && not isPositive) then
                1.0

            else
                0.3
    , if markType == Positive then
        "👍"

      else
        "👎"
    )


markView : MarkType -> Mark -> msg -> Element msg
markView markType isPositive msg =
    let
        ( a, label ) =
            markAlphaValue markType isPositive
    in
    button [ alpha a, width (px 50), height fill ] { label = centeredText label, onPress = Just msg }


type Size
    = None
    | Small
    | XSmall
    | Normal
    | Large
    | XLarge


size : Size -> Int
size s =
    case s of
        None ->
            0

        XSmall ->
            8

        Small ->
            12

        Normal ->
            16

        Large ->
            24

        XLarge ->
            32
