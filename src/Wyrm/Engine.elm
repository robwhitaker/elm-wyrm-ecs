module Wyrm.Engine exposing (..)

import AnimationFrame
import Focus exposing (Focus)
import Html exposing (Html)
import PageVisibility exposing (Visibility(..))
import Time exposing (Time)
import Wyrm exposing (..)



------ ENGINE ------


type Msg msg
    = WyrmMsg WyrmMsg
    | UserMsg msg


type WyrmMsg
    = Tick Time
    | VisibilityChange Visibility


type alias WyrmEngineInit flags userModel id components msg =
    { init : flags -> ( userModel, Cmd msg )
    , systems : List (System userModel id components msg)
    , update : msg -> userModel -> ( userModel, Cmd msg )
    , view : userModel -> Html msg
    , subscriptions : userModel -> Sub msg
    , gameStateFocus : Focus userModel (GameState id components)
    }


wyrmUpdate :
    (msg -> userModel -> ( userModel, Cmd msg ))
    -> List (System userModel id components msg)
    -> Focus userModel (GameState id components)
    -> Msg msg
    -> userModel
    -> ( userModel, Cmd (Msg msg) )
wyrmUpdate userUpdate systems gsFocus msg game =
    case msg of
        WyrmMsg msg_ ->
            case msg_ of
                Tick dt ->
                    let
                        (GameState { pageVisible }) =
                            Focus.get gsFocus game
                    in
                        if pageVisible then
                            let
                                (SystemRuntime systemRuntime) =
                                    runSystems gsFocus dt game systems
                            in
                                ( systemRuntime.model, Cmd.map UserMsg systemRuntime.cmds )

                        else
                            ( game, Cmd.none )

                VisibilityChange visibility ->
                    ( Focus.update gsFocus (\(GameState gs) -> GameState { gs | pageVisible = visibility == Visible }) game, Cmd.none )

        UserMsg msg_ ->
            let
                ( newState, cmds ) =
                    userUpdate msg_ game
            in
                ( newState, Cmd.map UserMsg cmds )


runGame : WyrmEngineInit flags userModel id components msg -> Program flags userModel (Msg msg)
runGame init =
    Html.programWithFlags
        { init = init.init >> (\( game, msg ) -> ( game, Cmd.map UserMsg msg ))
        , update = wyrmUpdate init.update init.systems init.gameStateFocus
        , view = init.view >> Html.map UserMsg
        , subscriptions =
            \game ->
                Sub.batch
                    [ Sub.map WyrmMsg (PageVisibility.visibilityChanges VisibilityChange)
                    , Sub.map WyrmMsg (AnimationFrame.diffs Tick)
                    , Sub.map UserMsg (init.subscriptions game)
                    ]
        }
