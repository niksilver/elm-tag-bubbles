module UI
    ( Model, Action(NoOp, NewTags), TaskOut
    , update, view
    , inputs
    ) where

-- Bubble forms captured as Html

import Constants exposing (TagsResult)
import Context exposing
    ( Context
    , CountedClick (NoClick, SingleClick, DoubleClick)
    )
import World
import TagFetcher
import RecentreButton

import Html exposing (Html, div, text)
import Svg exposing (svg)
import Svg.Attributes exposing (width, height, viewBox)
import Signal exposing (forwardTo)
import Task exposing (Task)
import Time exposing (Time, millisecond)
import Window

type alias Model =
    { width : Int
    , height : Int
    , world : World.Model
    , status : String
    }

type Action
    = Resize (Int, Int)
        | Direct World.Action
        | Tick Time
        | NewTags TagsResult
        | Click CountedClick
        | NoOp

-- A task to run a tag fetch... or maybe there's no task to run

type alias TaskOut = Maybe (Task () TagsResult)

-- Initial actions

ticker : Signal Action
ticker =
    Signal.map Tick (Time.every (30 * millisecond))

countedClicks : Signal Action
countedClicks =
    Context.mappedCountedClicks Click

resizing : Signal Action
resizing =
    Signal.map Resize Window.dimensions

inputs : List (Signal Action)
inputs = [resizing, ticker, countedClicks]

-- Update

update : Action -> Model -> (Model, TaskOut)
update action model =
    case action of
        Resize (w, h) ->
            ({ model | width = w, height = h }
             , Nothing
            )
        Direct act ->
            ({ model | world = World.update act model.world }
             , Nothing
            )
        Tick time ->
            ({ model | world = World.update (World.Tick time) model.world }
             , Nothing
            )
        NewTags tags ->
            case tags of
                Ok data ->
                    ({ model
                     | world = World.update (World.NewTags data) model.world
                     , status = "Done"
                     }
                     , Nothing
                    )
                Err error ->
                    ({ model
                     | status = "Error: " ++ (toString error)
                     }
                     , Nothing
                    )
        Click clicker ->
            case clicker of
                NoClick ->
                    ( model, Nothing )
                SingleClick _ ->
                    ( model, Nothing )
                DoubleClick tag ->
                    ( { model
                      | status = "Fetching " ++ (tag)
                      }
                    , Just (TagFetcher.getTags tag)
                    )
        NoOp ->
            (model, Nothing)

view : Signal.Address Action -> Model -> Html
view address model =
    let
        context = Context.create (Signal.forwardTo address Direct)
    in
        div []
        [ RecentreButton.view context (model.width, model.height)
        , svgView context model
        , text (model.status)
        ]

svgView : Context World.Action -> Model -> Html
svgView context model =
    svg
        [ width (model.width - 300 |> toString)
        , height (model.height - 50 |> toString)
        ]
        (World.view context model.world)

