module Wyrm exposing
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

import Monocle.Lens as Lens exposing (Lens)
import Dict exposing (Dict)

------ CORE ------

type alias Id = String

type GameState components
    = GameState
        { entities : Dict String (Entity components)
        , currentId : Int
        }


emptyGameState : GameState components
emptyGameState =
    GameState
        { entities = Dict.empty
        , currentId = 0
        }


------ ENTITY ------


type Entity components
    = Entity
        { id : Id
        , components : components
        }


getId : Entity components -> Id
getId (Entity { id }) =
    id


getComponents : Entity components -> components
getComponents (Entity { components }) =
    components


type EntityId 
    = WithCustomId Id
    | WithGeneratedId


{-| add an entity, optionally with a simple name
-}
addEntity : EntityId -> components -> GameState components -> GameState components
addEntity entityId components (GameState state) =
    GameState <|
        case entityId of
            WithCustomId id ->
                { state
                    | entities =
                        Dict.insert id
                            (Entity
                                { id = id
                                , components = components
                                }
                            )
                            state.entities
                }

            WithGeneratedId ->
                { state
                    | entities =
                        Dict.insert (String.fromInt state.currentId)
                            (Entity
                                { id = String.fromInt state.currentId
                                , components = components
                                }
                            )
                            state.entities
                    , currentId = state.currentId + 1
                }


{-| get an entity by Id
-}
getEntity : Id -> GameState components -> Maybe (Entity components)
getEntity id (GameState state) =
    Dict.get id state.entities


getEntities : GameState components -> Dict String (Entity components)
getEntities (GameState { entities }) =
    entities


{-| remove an entity by its Id.
-}
removeEntity : Id -> GameState components -> GameState components
removeEntity id (GameState state) =
    GameState { state | entities = Dict.remove id state.entities }


{-| update an entity by its Id
-}
updateEntity : Id -> (components -> components) -> GameState components -> GameState components
updateEntity id fn (GameState state) =
    let
        componentLens f (Entity e) =
            Entity { e | components = f e.components }
    in
    GameState { state | entities = Dict.update id (Maybe.map (componentLens fn)) state.entities }



------ SYSTEM ------


type SystemRuntime userModel components msg
    = SystemRuntime
        { model : userModel
        , cmds : Cmd msg
        , dt : Float
        , gsLens : Lens userModel (GameState components)
        }


systemRuntime : Lens userModel (GameState components) -> Float -> userModel -> SystemRuntime userModel components msg
systemRuntime gsLens dt userModel =
    SystemRuntime
        { model = userModel
        , dt = dt
        , cmds = Cmd.none
        , gsLens = gsLens
        }


fromSystemRuntime : SystemRuntime userModel components msg -> ( userModel, Cmd msg )
fromSystemRuntime (SystemRuntime sysrt) =
    ( sysrt.model, sysrt.cmds )


getUserModel : SystemRuntime userModel components msg -> userModel
getUserModel (SystemRuntime { model }) =
    model


getGameState : SystemRuntime userModel components msg -> GameState components
getGameState (SystemRuntime { model, gsLens }) =
    gsLens.get model


getDeltaTime : SystemRuntime userModel components msg -> Float
getDeltaTime (SystemRuntime { dt }) =
    dt


mapUserModel :
    (userModel -> userModel)
    -> SystemRuntime userModel components msg
    -> SystemRuntime userModel components msg
mapUserModel f (SystemRuntime sysrt) =
    SystemRuntime { sysrt | model = f sysrt.model }


mapGameState : (GameState components -> GameState components) -> SystemRuntime userModel components msg -> SystemRuntime userModel components msg
mapGameState f (SystemRuntime ({ model, gsLens } as sysrt)) =
    SystemRuntime { sysrt | model = Lens.modify gsLens f model }


withGameState :
    (GameState components -> ( value, GameState components ))
    -> SystemRuntime userModel components msg
    -> ( value, SystemRuntime userModel components msg )
withGameState f (SystemRuntime ({ model, gsLens } as sysrt)) =
    let
        ( value, newState ) =
            f (gsLens.get model)
    in
    ( value, SystemRuntime { sysrt | model = gsLens.set newState model } )


sendCmd : Cmd msg -> SystemRuntime userModel components msg -> SystemRuntime userModel components msg
sendCmd cmd (SystemRuntime sysrt) =
    --TODO: check on order of Cmd.batch
    SystemRuntime { sysrt | cmds = Cmd.batch [ cmd, sysrt.cmds ] }


type System userModel components msg
    = System (SystemRuntime userModel components msg -> SystemRuntime userModel components msg)


runSystems :
    Lens userModel (GameState components)
    -> Float
    -> userModel
    -> List (System userModel components msg)
    -> SystemRuntime userModel components msg
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
    (Entity components
     -> SystemRuntime userModel components msg
     -> Maybe (SystemRuntime userModel components msg)
    )
    -> SystemRuntime userModel components msg
    -> SystemRuntime userModel components msg
processEntities sysFn sysrt =
    processEntitiesWithAccumulator (\entity sysRT acc -> Maybe.map (\a -> (\b c -> ( b, c )) a ()) (sysFn entity sysRT)) sysrt ()


processEntitiesWithAccumulator :
    (Entity components
     -> SystemRuntime userModel components msg
     -> accumulator
     -> Maybe ( SystemRuntime userModel components msg, accumulator )
    )
    -> SystemRuntime userModel components msg
    -> accumulator
    -> SystemRuntime userModel components msg
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


matchEntity : Maybe (Entity components) -> Maybe (Entity components)
matchEntity =
    identity


with : Lens components (Maybe comp) -> Maybe (Entity components) -> Maybe ( Entity components, (comp -> r) -> r )
with c maybeEntity =
    case maybeEntity of
        Nothing ->
            Nothing

        Just ((Entity { components }) as entity) ->
            Maybe.map ((\b -> ( entity, b )) << (|>)) (c.get components)


andWith : Lens components (Maybe comp) -> Maybe ( Entity components, a -> comp -> r ) -> Maybe ( Entity components, a -> r )
andWith c =
    Maybe.andThen
        (\( (Entity { components }) as entity, cont ) ->
            Maybe.map (\component -> (\b -> ( entity, b )) <| \z -> (|>) component (cont z)) (c.get components)
        )


alsoMatchEntity : Maybe (Entity components) -> Maybe ( Entity components, a -> comp -> r ) -> Maybe ( Entity components, a -> comp -> r )
alsoMatchEntity maybeEntity maybeCont =
    Maybe.map2
        (\entity ( _, cont ) -> ( entity, cont ))
        maybeEntity
        maybeCont


processEntity : a -> Maybe ( Entity components, a -> r ) -> Maybe r
processEntity f maybeCont =
    Maybe.map ((|>) f << Tuple.second) maybeCont
