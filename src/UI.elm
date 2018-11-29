module UI exposing
    ( Model, Msg (..) --, TaskOut
    , update, view
    -- , inputs
    )

-- Bubble forms captured as Html

import Constants exposing (TagsResult(..))
-- import Context exposing
--     ( Context
--     , CountedClick (NoClick, SingleClick, DoubleClick)
--     )
import World
import TagFetcher
-- import NavBar
-- import Status exposing (Status)
-- import Help exposing (Help)

import Html exposing (Html, div)
import Html.Attributes exposing (class)
-- import Signal exposing (forwardTo)
-- import Task exposing (Task)
import Time exposing (Posix)
-- import Window


type alias Model =
    { dimensions : (Int, Int)
    , world : World.Model
    -- , status : Status
    -- , help : Help
    }

type Msg
   =
   -- Resize (Int, Int)
     Direct World.Msg
   | Tick Posix
   | NewTags TagsResult
   -- | Click CountedClick
   -- | StatusAction Status.Action
   -- | HelpAction Help
   | NoOp


-- A task to run a tag fetch... or maybe there's no task to run

-- type alias TaskOut = Maybe (Task () TagsResult)


-- Initial actions

-- ticker : Signal Action
-- ticker =
--     Signal.map Tick (Time.every (30 * millisecond))

-- countedClicks : Signal Action
-- countedClicks =
--     Context.mappedCountedClicks Click

-- resizing : Signal Action
-- resizing =
--     Signal.map Resize Window.dimensions

-- inputs : List (Signal Action)
-- inputs = [resizing, ticker, countedClicks]

-- Update

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
--     Resize dims ->
--         ({ model
--          | dimensions = dims
--          , world = World.update (World.Resize dims) model.world
--          }
--          , Nothing
--         )

    Direct msg_ ->
        ({ model | world = World.update msg_ model.world }
         , Cmd.none
        )

    Tick time ->
        ({ model | world = World.update (World.Tick time) model.world }
         , Cmd.none
        )

    NewTags tags ->
        case tags of
            TagsResult (Ok data) ->
                ({ model
                 | world = World.update (World.NewTags data) model.world
                 -- , status = Status.update (Status.Main "Ready") model.status
                 }
                 , Cmd.none
                )
            TagsResult (Err error) ->
--                 let
--                     statusMessage = "Error: " ++ (toString error)
--                     status = Status.update (Status.Main statusMessage) model.status
--                 in
                    -- ({ model | status = status }
                    (model
                     , Cmd.none
                    )

--     Click clicker ->
--         case clicker of
--             NoClick ->
--                 ( model, Nothing )
--             SingleClick _ ->
--                 ( model, Nothing )
--             DoubleClick tag ->
--                 let
--                     msg = "Fetching " ++ (tag.webTitle)
--                     status = Status.update (Status.Main msg) model.status
--                 in
--                     ( { model | status = status }
--                     , Just (TagFetcher.getTags tag.id)
--                     )
-- 
--     StatusAction statAct ->
--         ( { model | status = Status.update statAct model.status }
--         , Nothing
--         )
-- 
--     HelpAction onOff ->
--         ( { model | help = onOff }
--         , Nothing
--         )

    NoOp ->
        (model, Cmd.none)

view : Model -> Html Msg
view model =
    let
--         context' =
--             Context.create address
--                 |> Context.forwardStatus StatusAction
--         worldContext = Context.forwardTo context' Direct
--         helpContext = Context.forwardTo context' HelpAction
        world = model.world
    in
        div [ class "row" ]
        [ div [ class "sideBar" ] []
        , div [ class "column" ]
          [ -- NavBar.view worldContext helpContext world.scale
          -- , World.view worldContext world
            World.view world |> Html.map Direct
          -- , Status.view model.status
          ]
        , div [ class "sideBar" ] []
        -- , Help.view helpContext model.help
        ]

