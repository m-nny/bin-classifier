module Classifier exposing (..)

import Browser
import Data exposing (..)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input exposing (button)
import File exposing (File)
import File.Select as Select
import Html exposing (Html)
import Lego exposing (..)
import Task


main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type alias Model =
    { dataset : Dataset, rawDataset : String }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model emptyDataset "", Cmd.none )


type Msg
    = NoOp
    | UpdateEntry Int Mark
    | UpdateDataset Dataset
    | DatasetRequested
    | DatasetSelected File
    | DatasetLoaded String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        UpdateEntry id newMark ->
            let
                updateEntry t =
                    if t.id == id then
                        { t | mark = newMark }

                    else
                        t

                dataset =
                    model.dataset
            in
            ( { model | dataset = { dataset | entries = List.map updateEntry dataset.entries } }, Cmd.none )

        UpdateDataset newDataset ->
            ( { model | dataset = newDataset }, Cmd.none )

        DatasetRequested ->
            ( model, Select.file [ "text/json" ] DatasetSelected )

        DatasetSelected file ->
            ( model, Task.perform DatasetLoaded (File.toString file) )

        DatasetLoaded rawDataset ->
            let
                newDataset =
                    Result.withDefault emptyDataset (decodeDataset rawDataset)
            in
            ( { model | rawDataset = rawDataset, dataset = newDataset }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Html Msg
view model =
    Element.layout
        [ Background.color (rgb255 255 249 249) ]
        (column (rootAttributes <| rawView model) [ titleView model.dataset.title, dataView model.dataset.entries ])


rawView : Model -> Element Msg
rawView model =
    column (card [])
        [ button (card []) { label = centeredText "load", onPress = Just DatasetRequested }
        , centeredText model.rawDataset
        ]


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
