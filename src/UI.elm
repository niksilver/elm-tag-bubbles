module UI exposing
    ( Model, Msg (..)
    , update, view
    )


import Constants exposing (TagsResult(..))
import World
import TagFetcher
import NavBar
import Status exposing (Status)
import Help exposing (Help)
import Out
import Util

import Html exposing (Html, div)
import Html.Attributes exposing (class)
import Time exposing (Posix)


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
   | HelpMsg Help
   | ToNavBar NavBar.Msg
   | NoOp


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
          (world, outMsg) = World.update msg_ model.world
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
                let
                    statusMessage = Util.httpErrorToString error
                    status = Status.update (Status.Main statusMessage) model.status
                in
                    ( { model | status = status }
                    , Cmd.none
                    )

    ToNavBar navMsg ->
        let
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
  case outMsg of
    Out.None ->
      (model, cmd)

    actualOutMsg ->
      let
          (model2, cmd2, out2) = updateFromOutMsg actualOutMsg model
      in
          updateFromOutMsgs out2 (Cmd.batch [cmd, cmd2]) model2


updateFromOutMsg : Out.Msg -> Model -> (Model, Cmd Msg, Out.Msg)
updateFromOutMsg outMsg model =
  case outMsg of
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
          notice = "Fetching " ++ (tag.webTitle)
          outMsg2 = Out.StatusMsg (Status.Main notice)
          cmd = TagFetcher.getTags tag.id |> Cmd.map NewTags
      in
          (model, cmd, outMsg2)

    Out.Recentre ->
      let
          (world, outMsg2) = World.update World.Recentre model.world
          model2 = { model | world = world }
      in
          (model2, Cmd.none, outMsg2)

    Out.Scale factor ->
      let
          (world, outMsg2) = World.update (World.Scale factor) model.world
          model2 = { model | world = world }
      in
          (model2, Cmd.none, outMsg2)

    Out.None ->
      (model, Cmd.none, Out.None)


view : Model -> Html Msg
view model =
    let
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

