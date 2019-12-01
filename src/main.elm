module Classifier exposing (..)

import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font


main =
    Element.layout
        [ Background.color (rgb255 255 249 249)
        ]
        view


view : Element msg
view =
    column
        [ width (fill |> maximum 600)
        , height fill
        , centerX
        , spacing (size Normal)
        , padding (size Normal)
        ]
        [ el (card [ height <| px 80 ]) (centeredText "Is String email?")
        , dataView
        ]


dataView : Element msg
dataView =
    column (card [ spacing (size Small), padding (size Small), height fill, Background.color (rgb255 238 226 223) ])
        [ entryView "m-nny@"
        , entryView "m-nny@yandex.ru"
        ]


entryView : String -> Element msg
entryView query =
    el
        (card
            [ height <| px 60
            , width (shrink |> minimum 400)
            , paddingXY (size Small) 0
            ]
        )
        (centeredText query)


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


type Size
    = None
    | Small
    | Normal
    | Large


size : Size -> Int
size s =
    case s of
        None ->
            0

        Small ->
            8

        Normal ->
            16

        Large ->
            32
