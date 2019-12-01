module Classifier exposing (Model, Msg, init, subscriptions, update, view)

import Browser
import Data exposing (..)
import Element exposing (Element, el, padding, spacing, text)
import Element.Input exposing (button)
import Utils


main =
    Browser.element
        { init = init
        , view = \model -> Element.layout [] (view model)
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { dataset : Dataset }


type Msg
    = UpdateDataset Dataset
    | UpdateMark Int Bool


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateDataset newDataset ->
            ( { model | dataset = newDataset }, Cmd.none )

        UpdateMark index value ->
            ( { model | dataset = datasetMarkEntry index value model.dataset }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model (Dataset "title" [ "гороскоп", "рецепты" ] [ Nothing, Nothing ]), Cmd.none )


view : Model -> Element Msg
view model =
    Element.column [ spacing 16, padding 16 ]
        [ el [] (text model.dataset.description)
        , datasetView model.dataset
        ]


datasetView : Dataset -> Element Msg
datasetView dataset =
    Element.column [ spacing 4 ]
        (List.indexedMap entryView (List.map2 Tuple.pair dataset.entries dataset.marks))


entryView : Int -> ( String, Maybe Bool ) -> Element Msg
entryView index ( query, mark ) =
    Element.row [ spacing 8 ]
        [ text query
        , text (markToString mark)
        , button [] { onPress = Just (UpdateMark index True), label = text "positive" }
        , button [] { onPress = Just (UpdateMark index False), label = text "negative" }
        ]


markToString : Maybe Bool -> String
markToString mark =
    case mark of
        Nothing ->
            "??"

        Just value ->
            if value then
                "+"

            else
                "-"
