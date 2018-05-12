module Wyrm.Engine exposing (..)

import AnimationFrame
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
    { init : flags -> ( Game userModel id components, Cmd msg )
    , systems : List (System userModel id components msg)
    , update : msg -> Game userModel id components -> ( Game userModel id components, Cmd msg )
    , view : Game userModel id components -> Html msg
    , subscriptions : Game userModel id components -> Sub msg
    }


wyrmUpdate :
    (msg -> Game userModel id components -> ( Game userModel id components, Cmd msg ))
    -> List (System userModel id components msg)
    -> Msg msg
    -> Game userModel id components
    -> ( Game userModel id components, Cmd (Msg msg) )
wyrmUpdate userUpdate systems msg game =
    case msg of
        WyrmMsg msg_ ->
            case msg_ of
                Tick dt ->
                    let
                        (GameState { pageVisible }) =
                            game.wyrmGameState
                    in
                        if pageVisible then
                            let
                                (SystemRuntime systemRuntime) =
                                    runSystems dt game systems
                            in
                                ( systemRuntime.model, Cmd.map UserMsg systemRuntime.cmds )

                        else
                            ( game, Cmd.none )

                VisibilityChange visibility ->
                    let
                        (GameState wyrmGameState) =
                            game.wyrmGameState
                    in
                        ( { game | wyrmGameState = GameState { wyrmGameState | pageVisible = visibility == Visible } }, Cmd.none )

        UserMsg msg_ ->
            let
                ( newState, cmds ) =
                    userUpdate msg_ game
            in
                ( newState, Cmd.map UserMsg cmds )


runGame : WyrmEngineInit flags userModel id components msg -> Program flags (Game userModel id components) (Msg msg)
runGame init =
    Html.programWithFlags
        { init = init.init >> (\( game, msg ) -> ( game, Cmd.map UserMsg msg ))
        , update = wyrmUpdate init.update init.systems
        , view = init.view >> Html.map UserMsg
        , subscriptions =
            \game ->
                Sub.batch
                    [ Sub.map WyrmMsg (PageVisibility.visibilityChanges VisibilityChange)
                    , Sub.map WyrmMsg (AnimationFrame.diffs Tick)
                    , Sub.map UserMsg (init.subscriptions game)
                    ]
        }
