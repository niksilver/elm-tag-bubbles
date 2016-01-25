module UI
    ( Model
    , update, view
    , initialEffects, inputs
    ) where

-- Bubble forms captured as Html

import Constants exposing (TagsResult)
import Context exposing
    ( Context
    , CountedClick (NoClick, SingleClick, DoubleClick)
    )
import World
import RecentreButton
import TagFetcher

import Html exposing (Html, div, text)
import Svg exposing (svg)
import Svg.Attributes exposing (width, height, viewBox)
import Signal exposing (forwardTo)
import Effects exposing (Effects)
import Time exposing (Time, millisecond)

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
        | Recentre

-- Initial effects

initialEffects : Effects Action
initialEffects =
    Effects.none

-- Initial actions

ticker : Signal Action
ticker =
    Signal.map Tick (Time.every (30 * millisecond))

countedClicks : Signal Action
countedClicks =
    Context.mappedCountedClicks Click

recentring : Signal Action
recentring =
    Signal.map (always Recentre) Context.recentreClicks

inputs : List (Signal Action)
inputs = [ticker, countedClicks, recentring]

-- Update

update : Action -> Model -> (Model, Effects Action)
update action model =
    case action of
        Resize (w, h) ->
            ({ model | width = w, height = h }
             , Effects.none
            )
        Direct act ->
            ({ model | world = World.update act model.world }
             , Effects.none
            )
        Tick time ->
            ({ model | world = World.update (World.Tick time) model.world }
             , Effects.none
            )
        NewTags tags ->
            case tags of
                Ok data ->
                    ({ model
                     | world = World.update (World.NewTags data) model.world
                     , status = "Done"
                     }
                     , Effects.none
                    )
                Err error ->
                    ({ model
                     | status = "Error: " ++ (toString error)
                     }
                     , Effects.none
                    )
        Click clicker ->
            case clicker of
                NoClick ->
                    ( model, Effects.none )
                SingleClick _ ->
                    ( model, Effects.none )
                DoubleClick tag ->
                    ( { model
                      | status = "Fetching " ++ (tag)
                      }
                    , Effects.map NewTags (TagFetcher.getTags tag)
                    )
        Recentre ->
            ({ model
             | status = "Recentre me!"
             }
            , Effects.none
            )

view : Signal.Address Action -> Model -> Html
view address model =
    let
        context = Context.create (Signal.forwardTo address Direct)
    in
        div []
        [ RecentreButton.view context
        , svgView context model
        , text (model.status)
        ]

svgView : Context World.Action -> Model -> Html
svgView context model =
    svg
        [ width (toString model.width)
        , height (toString model.height)
        ]
        (World.view context model.world)

