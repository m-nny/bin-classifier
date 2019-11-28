module Classifier exposing (Model, Msg, init, subscriptions, update, view)

import Browser
import Data exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Utils


main =
    Browser.element
        { init = init
        , view = view
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


view : Model -> Html Msg
view model =
    div [ class "root" ]
        [ div [ class "title" ]
            [ div [] [ text model.dataset.description ]
            ]
        , datasetView model.dataset
        ]


datasetView : Dataset -> Html Msg
datasetView dataset =
    div [ class "dataset" ]
        (List.indexedMap entryView (List.map2 Tuple.pair dataset.entries dataset.marks))


entryView : Int -> (String, Maybe Bool) -> Html Msg
entryView index (query, mark) =
    div [ class "dataset__entry" ]
        [ text query
        , text (markToString mark)
        , button [ onClick (UpdateMark index True) ] [ text "positive" ]
        , button [ onClick (UpdateMark index False) ] [ text "negative" ]
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
