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
import NavBar
import Status exposing (Status, Action(Main))

import Html exposing (Html, div)
import Html.Attributes exposing (class)
import Signal exposing (forwardTo)
import Task exposing (Task)
import Time exposing (Time, millisecond)
import Window

type alias Model =
    { dimensions : (Int, Int)
    , world : World.Model
    , status : Status
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
        Resize dims ->
            ({ model
             | dimensions = dims
             , world = World.update (World.Resize dims) model.world
             }
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
                     , status = (Status.update (Main "Done") model.status)
                     }
                     , Nothing
                    )
                Err error ->
                    let
                        msg = "Error: " ++ (toString error)
                        status = Status.update (Main msg) model.status
                    in
                        ({ model | status = status }
                         , Nothing
                        )
        Click clicker ->
            case clicker of
                NoClick ->
                    ( model, Nothing )
                SingleClick _ ->
                    ( model, Nothing )
                DoubleClick tag ->
                    let
                        msg = "Fetching " ++ (tag.webTitle)
                        status = Status.update (Main msg) model.status
                    in
                        ( { model | status = status }
                        , Just (TagFetcher.getTags tag.id)
                        )
        NoOp ->
            (model, Nothing)

view : Signal.Address Action -> Model -> Html
view address model =
    let
        context = Context.create (Signal.forwardTo address Direct)
        world = model.world
    in
        div [ class "column" ]
        [ NavBar.view context world.scale
        , div [ class "row" ]
          [ div [ class "sideBar" ] []
          , World.view context world
          , div [ class "sideBar" ] []
          ]
        , Status.view model.status
        ]

