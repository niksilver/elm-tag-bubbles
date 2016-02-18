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
import Status exposing (Status)
import Help

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
        | StatusAction Status.Action
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
                     , status = Status.update (Status.Main "Ready") model.status
                     }
                     , Nothing
                    )
                Err error ->
                    let
                        msg = "Error: " ++ (toString error)
                        status = Status.update (Status.Main msg) model.status
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
                        status = Status.update (Status.Main msg) model.status
                    in
                        ( { model | status = status }
                        , Just (TagFetcher.getTags tag.id)
                        )
        StatusAction statAct ->
            ( { model | status = Status.update statAct model.status }
            , Nothing)
        NoOp ->
            (model, Nothing)

view : Signal.Address Action -> Model -> Html
view address model =
    let
        context' =
            Context.create address
                |> Context.forwardStatus StatusAction
        context = Context.forwardTo context' Direct
        world = model.world
    in
        div [ class "row" ]
        [ div [ class "sideBar" ] []
        , div [ class "column" ]
          [ NavBar.view context world.scale
          , World.view context world
          , Status.view model.status
          ]
        , div [ class "sideBar" ] []
        , Help.view
        ]

