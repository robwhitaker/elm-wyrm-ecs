module Wyrm
    exposing
        ( Entity
        , EntityId(..)
        , GameState
        , System
        , SystemRuntime
        , addEntity
        , alsoMatchEntity
        , andWith
        , emptyGameState
        , fromSystemRuntime
        , getComponents
        , getDeltaTime
        , getEntities
        , getEntity
        , getGameState
        , getId
        , getUserModel
        , mapGameState
        , mapUserModel
        , matchEntity
        , processEntities
        , processEntitiesWithAccumulator
        , processEntity
        , removeEntity
        , runSystems
        , sendCmd
        , systemRuntime
        , updateEntity
        , with
        , withGameState
        )

import Dict exposing (Dict)
import Focus exposing (Focus)
import Time exposing (Time)



------ CORE ------


type GameState id components
    = GameState
        { entities : Dict String (Entity id components)
        , currentId : Int
        , pageVisible : Bool
        }


emptyGameState : GameState id components
emptyGameState =
    GameState
        { entities = Dict.empty
        , currentId = 0
        , pageVisible = True
        }



------ ENTITY ------


type Entity id components
    = Entity
        { id : id
        , components : components
        }


getId : Entity id components -> id
getId (Entity { id }) =
    id


getComponents : Entity id components -> components
getComponents (Entity { components }) =
    components


type EntityId id
    = WithCustomId id
    | WithGeneratedId (Int -> id)


{-| add an entity, optionally with a simple name
-}
addEntity : EntityId id -> components -> GameState id components -> GameState id components
addEntity entityId components (GameState state) =
    GameState <|
        case entityId of
            WithCustomId id ->
                { state
                    | entities =
                        Dict.insert (toString id)
                            (Entity
                                { id = id
                                , components = components
                                }
                            )
                            state.entities
                }

            WithGeneratedId mkId ->
                { state
                    | entities =
                        Dict.insert (toString (mkId state.currentId))
                            (Entity
                                { id = mkId state.currentId
                                , components = components
                                }
                            )
                            state.entities
                    , currentId = state.currentId + 1
                }


{-| get an entity by id
-}
getEntity : id -> GameState id components -> Maybe (Entity id components)
getEntity id (GameState state) =
    Dict.get (toString id) state.entities


getEntities : GameState id components -> Dict String (Entity id components)
getEntities (GameState { entities }) =
    entities


{-| remove an entity by its id.
-}
removeEntity : id -> GameState id components -> GameState id components
removeEntity id (GameState state) =
    GameState { state | entities = Dict.remove (toString id) state.entities }


{-| update an entity by its id
-}
updateEntity : id -> (components -> components) -> GameState id components -> GameState id components
updateEntity id fn (GameState state) =
    let
        componentFocus f (Entity e) =
            Entity { e | components = f e.components }
    in
        GameState { state | entities = Dict.update (toString id) (Maybe.map (componentFocus fn)) state.entities }



------ SYSTEM ------


type SystemRuntime userModel id components msg
    = SystemRuntime
        { model : userModel
        , cmds : Cmd msg
        , dt : Time
        , gsFocus : Focus userModel (GameState id components)
        }


systemRuntime : Focus userModel (GameState id components) -> Time -> userModel -> SystemRuntime userModel id components msg
systemRuntime gsFocus dt userModel =
    SystemRuntime
        { model = userModel
        , dt = dt
        , cmds = Cmd.none
        , gsFocus = gsFocus
        }


fromSystemRuntime : SystemRuntime userModel id components msg -> ( userModel, Cmd msg )
fromSystemRuntime (SystemRuntime systemRuntime) =
    ( systemRuntime.model, systemRuntime.cmds )


getUserModel : SystemRuntime userModel id components msg -> userModel
getUserModel (SystemRuntime { model }) =
    model


getGameState : SystemRuntime userModel id components msg -> GameState id components
getGameState (SystemRuntime { model, gsFocus }) =
    Focus.get gsFocus model


getDeltaTime : SystemRuntime userModel id components msg -> Time
getDeltaTime (SystemRuntime { dt }) =
    dt


mapUserModel :
    (userModel -> userModel)
    -> SystemRuntime userModel id components msg
    -> SystemRuntime userModel id components msg
mapUserModel f (SystemRuntime systemRuntime) =
    SystemRuntime { systemRuntime | model = f systemRuntime.model }


mapGameState : (GameState id components -> GameState id components) -> SystemRuntime userModel id components msg -> SystemRuntime userModel id components msg
mapGameState f (SystemRuntime ({ model, gsFocus } as systemRuntime)) =
    SystemRuntime { systemRuntime | model = Focus.update gsFocus f model }


withGameState :
    (GameState id components -> ( value, GameState id components ))
    -> SystemRuntime userModel id components msg
    -> ( value, SystemRuntime userModel id components msg )
withGameState f (SystemRuntime ({ model, gsFocus } as systemRuntime)) =
    let
        ( value, newState ) =
            f (Focus.get gsFocus model)
    in
        ( value, SystemRuntime { systemRuntime | model = Focus.set gsFocus newState model } )


sendCmd : Cmd msg -> SystemRuntime userModel id components msg -> SystemRuntime userModel id components msg
sendCmd cmd (SystemRuntime systemRuntime) =
    --TODO: check on order of Cmd.batch
    SystemRuntime { systemRuntime | cmds = Cmd.batch [ cmd, systemRuntime.cmds ] }


type System userModel id components msg
    = System (SystemRuntime userModel id components msg -> SystemRuntime userModel id components msg)


runSystems :
    Focus userModel (GameState id components)
    -> Time
    -> userModel
    -> List (System userModel id components msg)
    -> SystemRuntime userModel id components msg
runSystems gsFocus dt userModel systems =
    List.foldl
        (\(System sysFn) systemRuntime ->
            sysFn systemRuntime
        )
        (SystemRuntime
            { model = userModel
            , dt = dt
            , cmds = Cmd.none
            , gsFocus = gsFocus
            }
        )
        systems


processEntities :
    (Entity id components
     -> SystemRuntime userModel id components msg
     -> Maybe (SystemRuntime userModel id components msg)
    )
    -> SystemRuntime userModel id components msg
    -> SystemRuntime userModel id components msg
processEntities sysFn systemRuntime =
    processEntitiesWithAccumulator (\entity sysRT acc -> Maybe.map (flip (,) ()) (sysFn entity sysRT)) systemRuntime ()


processEntitiesWithAccumulator :
    (Entity id components
     -> SystemRuntime userModel id components msg
     -> accumulator
     -> Maybe ( SystemRuntime userModel id components msg, accumulator )
    )
    -> SystemRuntime userModel id components msg
    -> accumulator
    -> SystemRuntime userModel id components msg
processEntitiesWithAccumulator sysFn (SystemRuntime startingRuntime) startingAcc =
    let
        getEntities gs =
            let
                (GameState gameState) =
                    gs
            in
                gameState.entities
    in
        Dict.foldl
            (\id _ ( (SystemRuntime ({ model, gsFocus } as systemRuntime)) as runtime, acc ) ->
                case Dict.get id (getEntities (Focus.get gsFocus model)) of
                    Nothing ->
                        ( runtime, acc )

                    Just currentEntity ->
                        let
                            maybeNewRuntime =
                                sysFn currentEntity runtime acc
                        in
                            case maybeNewRuntime of
                                Nothing ->
                                    ( runtime, acc )

                                Just newRuntime ->
                                    newRuntime
            )
            ( SystemRuntime startingRuntime, startingAcc )
            (getEntities (Focus.get startingRuntime.gsFocus startingRuntime.model))
            |> Tuple.first



------ COMPONENT ------
-- Helpers for matching entities


matchEntity : Maybe (Entity id components) -> Maybe (Entity id components)
matchEntity =
    identity


with : Focus components (Maybe comp) -> Maybe (Entity id components) -> Maybe ( Entity id components, (comp -> r) -> r )
with c maybeEntity =
    case maybeEntity of
        Nothing ->
            Nothing

        Just ((Entity { components }) as entity) ->
            Maybe.map ((,) entity << (|>)) (Focus.get c components)


andWith : Focus components (Maybe comp) -> Maybe ( Entity id components, a -> comp -> r ) -> Maybe ( Entity id components, a -> r )
andWith c =
    Maybe.andThen
        (\( (Entity { components }) as entity, cont ) ->
            Maybe.map (\component -> (,) entity <| \c -> (|>) component (cont c)) (Focus.get c components)
        )


alsoMatchEntity : Maybe (Entity id components) -> Maybe ( Entity id components, a -> comp -> r ) -> Maybe ( Entity id components, a -> comp -> r )
alsoMatchEntity maybeEntity maybeCont =
    Maybe.map2
        (\entity ( _, cont ) -> ( entity, cont ))
        maybeEntity
        maybeCont


processEntity : a -> Maybe ( Entity id components, a -> r ) -> Maybe r
processEntity f maybeCont =
    Maybe.map ((|>) f << Tuple.second) maybeCont
