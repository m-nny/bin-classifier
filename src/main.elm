module Classifier exposing (..)

import Browser
import Data exposing (..)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Html exposing (Html)
import Lego exposing (..)


main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }


type alias Model =
    Dataset


init : Dataset
init =
    [ newEntry "m-nny@" 0, newEntry "m-nny@yandex.ru" 1 ]


type Msg
    = NoOp
    | UpdateEntry Int Mark


update : Msg -> Model -> Model
update msg model =
    case msg of
        NoOp ->
            model

        UpdateEntry id newMark ->
            let
                updateEntry t =
                    if t.id == id then
                        { t | mark = newMark }

                    else
                        t
            in
            List.map updateEntry model


view : Model -> Html Msg
view model =
    Element.layout
        [ Background.color (rgb255 255 249 249) ]
        (column rootAttributes [ titleView "Is String email?", dataView model ])


titleView : String -> Element Msg
titleView title =
    el (card [ height <| px 80, Font.italic, Font.size (size Large) ]) (centeredText title)


dataView : List Entry -> Element Msg
dataView entries =
    column (card [ spacing (size Small), padding (size Small), height fill, Background.color (rgb255 238 226 223) ])
        (List.map entryView entries)


entryView : Entry -> Element Msg
entryView entry =
    row
        (card
            [ height <| px 60
            , width (shrink |> minimum 400)
            ]
        )
        [ markView Negative entry.mark (UpdateEntry entry.id (Just False))
        , centeredText entry.query
        , markView Positive entry.mark (UpdateEntry entry.id (Just True))
        ]
