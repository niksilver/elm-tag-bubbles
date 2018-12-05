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
import NavBar
import Status exposing (Status)
import Help exposing (Help)
import Out

import Html exposing (Html, div)
import Html.Attributes exposing (class)
-- import Signal exposing (forwardTo)
-- import Task exposing (Task)
import Time exposing (Posix)
-- import Window


type alias Model =
    { dimensions : (Int, Int)
    , world : World.Model
    , status : Status
    , help : Help
    }

type Msg
   = Resize Int Int
   | Direct World.Msg
   | Tick Posix
   | NewTags TagsResult
   -- | Click CountedClick
   -- | StatusAction Status.Action
   | HelpMsg Help
   | ToNavBar NavBar.Msg
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
    Resize width height ->
        let
            (world, _) = World.update (World.Resize width height) model.world
        in
          ({ model
           | dimensions = (width, height)
           , world = world
           }
           , Cmd.none
          )

    Direct msg_ ->
      let
          x = Debug.log "UI.update case Direct (i), msg_ =" msg_
          (world, outMsg) = World.update msg_ model.world
          y = Debug.log "UI.update case Direct (ii), outMsg =" outMsg
          model2 = { model | world = world }
      in
          updateFromOutMsgs outMsg Cmd.none model2

    Tick time ->
        let
            (world, _) = World.update (World.Tick time) model.world
        in
            ({ model | world = world }
             , Cmd.none
            )

    NewTags tags ->
        let x = Debug.log "UI.update case NewTags" tags in
        case tags of
            TagsResult (Ok data) ->
                let
                    (world, _) = World.update (World.NewTags data) model.world
                in
                  ({ model
                   | world = world
                   , status = Status.update (Status.Main "Ready") model.status
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

    ToNavBar navMsg ->
        let
            x = Debug.log "UI.update case ToNavBar" navMsg
            outMsg = NavBar.update navMsg
        in
            updateFromOutMsgs outMsg Cmd.none model

    HelpMsg onOff ->
        ( { model | help = onOff }
        , Cmd.none
        )

    NoOp ->
        (model, Cmd.none)

updateFromOutMsgs : Out.Msg -> Cmd Msg -> Model -> (Model, Cmd Msg)
updateFromOutMsgs outMsg cmd model =
  let
      y = (Debug.log "UI.updateFromOutMsgs outMsg=" outMsg)
  in
  case outMsg of
    Out.None ->
        let x = Debug.log "UI.updateFromOutMsgs case, Out.None clause" outMsg in
      (model, cmd)

    actualOutMsg ->
      let
          x = Debug.log "UI.updateFromOutMsgs case, actualOutMsg clause" actualOutMsg
          (model2, cmd2, out2) = updateFromOutMsg actualOutMsg model
      in
          updateFromOutMsgs out2 (Cmd.batch [cmd, cmd2]) model2


updateFromOutMsg : Out.Msg -> Model -> (Model, Cmd Msg, Out.Msg)
updateFromOutMsg outMsg model =
  case (Debug.log "UI.updateFromOutMsg case " outMsg) of
    Out.StatusMsg sMsg ->
      ( { model | status = Status.update sMsg model.status }
      , Cmd.none
      , Out.None
      )

    Out.HelpMsg hMsg ->
      ( { model | help = hMsg }
      , Cmd.none
      , Out.None
      )

    Out.RequestTag tag ->
      let
          x = Debug.log "UI.updateFromOutMsg case, Out.RequestTag clause, tag is" tag
          notice = "Fetching " ++ (tag.webTitle)
          outMsg2 = Out.StatusMsg (Status.Main notice)
          cmd = TagFetcher.getTags tag.id |> Cmd.map NewTags
          y = Debug.log "UI.updateFromOutMsg case, Out.RequestTag clause cmd is" cmd
      in
          (model, cmd, outMsg2)

    Out.None ->
      (model, Cmd.none, Out.None)


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
          [ NavBar.view world.scale |> Html.map ToNavBar
          , World.view world |> Html.map Direct
          , Status.view model.status
          ]
        , div [ class "sideBar" ] []
        , Help.view model.help |> Html.map HelpMsg
        ]

