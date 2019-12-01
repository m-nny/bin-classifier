module Classifier exposing (Model, Msg, init, subscriptions, update, view)

import Browser
import Data exposing (..)
import Element exposing (..)
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
    Element.column [ spacing 16, padding 16, centerX, width (fill |> maximum 400) ]
        [ el [ centerX ] (text model.dataset.description)
        , datasetView model.dataset
        ]


datasetView : Dataset -> Element Msg
datasetView dataset =
    let
        data =
            List.map2 Tuple.pair dataset.entries dataset.marks
    in
    Element.indexedTable [ spacing 8 ]
        { data = data
        , columns =
            [ { header = text "query", width = fill, view = entryView Query }
            , { header = text "mark", width = fill, view = entryView Mark }
            , { header = Element.none, width = fill, view = entryView MakeNegative }
            , { header = Element.none, width = fill, view = entryView MakePositive }
            ]
        }


type FieldName
    = Query
    | Mark
    | MakePositive
    | MakeNegative


entryView : FieldName -> Int -> ( String, Maybe Bool ) -> Element Msg
entryView field index ( query, mark ) =
    case field of
        Query ->
            text query

        Mark ->
            text (markToString mark)

        MakePositive ->
            button [] { onPress = Just (UpdateMark index True), label = text "positive" }

        MakeNegative ->
            button [] { onPress = Just (UpdateMark index False), label = text "negative" }


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
