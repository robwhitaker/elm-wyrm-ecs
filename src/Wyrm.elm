module Wyrm exposing
    ( Entity
    , EntityId(..)
    , GameState
    , System
    , SystemRuntime
    , addEntity
    , alsoMatchEntity
    , andWith
    , fromSystemRuntime
    , getComponents
    , getDeltaTime
    , getEntities
    , getEntity
    , getGameState
    , getId
    , getUserModel
    , initGameState
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
import Monocle.Lens as Lens exposing (Lens)



------ CORE ------


type GameState id components
    = GameState
        { entities : Dict String (Entity id components)
        , currentId : Int
        , mkComparableId : id -> String
        }


initGameState : (id -> String) -> GameState id components
initGameState mkComparableId =
    GameState
        { entities = Dict.empty
        , currentId = 0
        , mkComparableId = mkComparableId
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
                        Dict.insert (state.mkComparableId id)
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
                        Dict.insert (state.mkComparableId <| mkId state.currentId)
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
    Dict.get (state.mkComparableId id) state.entities


getEntities : GameState id components -> Dict String (Entity id components)
getEntities (GameState { entities }) =
    entities


{-| remove an entity by its id.
-}
removeEntity : id -> GameState id components -> GameState id components
removeEntity id (GameState state) =
    GameState { state | entities = Dict.remove (state.mkComparableId id) state.entities }


{-| update an entity by its id
-}
updateEntity : id -> (components -> components) -> GameState id components -> GameState id components
updateEntity id fn (GameState state) =
    let
        componentLens f (Entity e) =
            Entity { e | components = f e.components }
    in
    GameState { state | entities = Dict.update (state.mkComparableId id) (Maybe.map (componentLens fn)) state.entities }



------ SYSTEM ------


type SystemRuntime userModel id components msg
    = SystemRuntime
        { model : userModel
        , cmds : Cmd msg
        , dt : Float
        , gsLens : Lens userModel (GameState id components)
        }


systemRuntime : Lens userModel (GameState id components) -> Float -> userModel -> SystemRuntime userModel id components msg
systemRuntime gsLens dt userModel =
    SystemRuntime
        { model = userModel
        , dt = dt
        , cmds = Cmd.none
        , gsLens = gsLens
        }


fromSystemRuntime : SystemRuntime userModel id components msg -> ( userModel, Cmd msg )
fromSystemRuntime (SystemRuntime sysrt) =
    ( sysrt.model, sysrt.cmds )


getUserModel : SystemRuntime userModel id components msg -> userModel
getUserModel (SystemRuntime { model }) =
    model


getGameState : SystemRuntime userModel id components msg -> GameState id components
getGameState (SystemRuntime { model, gsLens }) =
    gsLens.get model


getDeltaTime : SystemRuntime userModel id components msg -> Float
getDeltaTime (SystemRuntime { dt }) =
    dt


mapUserModel :
    (userModel -> userModel)
    -> SystemRuntime userModel id components msg
    -> SystemRuntime userModel id components msg
mapUserModel f (SystemRuntime sysrt) =
    SystemRuntime { sysrt | model = f sysrt.model }


mapGameState : (GameState id components -> GameState id components) -> SystemRuntime userModel id components msg -> SystemRuntime userModel id components msg
mapGameState f (SystemRuntime ({ model, gsLens } as sysrt)) =
    SystemRuntime { sysrt | model = Lens.modify gsLens f model }


withGameState :
    (GameState id components -> ( value, GameState id components ))
    -> SystemRuntime userModel id components msg
    -> ( value, SystemRuntime userModel id components msg )
withGameState f (SystemRuntime ({ model, gsLens } as sysrt)) =
    let
        ( value, newState ) =
            f (gsLens.get model)
    in
    ( value, SystemRuntime { sysrt | model = gsLens.set newState model } )


sendCmd : Cmd msg -> SystemRuntime userModel id components msg -> SystemRuntime userModel id components msg
sendCmd cmd (SystemRuntime sysrt) =
    --TODO: check on order of Cmd.batch
    SystemRuntime { sysrt | cmds = Cmd.batch [ cmd, sysrt.cmds ] }


type System userModel id components msg
    = System (SystemRuntime userModel id components msg -> SystemRuntime userModel id components msg)


runSystems :
    Lens userModel (GameState id components)
    -> Float
    -> userModel
    -> List (System userModel id components msg)
    -> SystemRuntime userModel id components msg
runSystems gsLens dt userModel systems =
    List.foldl
        (\(System sysFn) sysrt ->
            sysFn sysrt
        )
        (SystemRuntime
            { model = userModel
            , dt = dt
            , cmds = Cmd.none
            , gsLens = gsLens
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
processEntities sysFn sysrt =
    processEntitiesWithAccumulator (\entity sysRT acc -> Maybe.map (\a -> (\b c -> ( b, c )) a ()) (sysFn entity sysRT)) sysrt ()


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
        getEntities_ gs =
            let
                (GameState gameState) =
                    gs
            in
            gameState.entities
    in
    Dict.foldl
        (\id _ ( (SystemRuntime ({ model, gsLens } as sysrt)) as runtime, acc ) ->
            case Dict.get id (getEntities_ (gsLens.get model)) of
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
        (getEntities_ (startingRuntime.gsLens.get startingRuntime.model))
        |> Tuple.first



------ COMPONENT ------
-- Helpers for matching entities


matchEntity : Maybe (Entity id components) -> Maybe (Entity id components)
matchEntity =
    identity


with : Lens components (Maybe comp) -> Maybe (Entity id components) -> Maybe ( Entity id components, (comp -> r) -> r )
with c maybeEntity =
    case maybeEntity of
        Nothing ->
            Nothing

        Just ((Entity { components }) as entity) ->
            Maybe.map ((\b -> ( entity, b )) << (|>)) (c.get components)


andWith : Lens components (Maybe comp) -> Maybe ( Entity id components, a -> comp -> r ) -> Maybe ( Entity id components, a -> r )
andWith c =
    Maybe.andThen
        (\( (Entity { components }) as entity, cont ) ->
            Maybe.map (\component -> (\b -> ( entity, b )) <| \z -> (|>) component (cont z)) (c.get components)
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
