module Wyrm exposing (..)

import Dict exposing (Dict)
import InfiniteStream as Stream exposing (Stream)
import Random.Pcg.Extended as Random exposing (Generator, Seed)
import Time exposing (Time)
import UuidStream exposing (UuidStream)



------ CORE ------


type alias Game userModel id components =
    { userModel | wyrmGameState : GameState id components }


type GameState id components
    = GameState
        { entities : Dict String (Entity id components)
        , idStream : UuidStream String
        , randomSeedStream : Stream Seed
        , pageVisible : Bool
        }


seededGameState : Int -> List Int -> GameState id components
seededGameState firstInt extensions =
    GameState
        { entities = Dict.empty
        , idStream = UuidStream.uuidStringStream firstInt extensions
        , randomSeedStream =
            let
                ( startingSeed, _ ) =
                    Random.step Random.independentSeed (Random.initialSeed firstInt extensions)
            in
                Stream.iterate (Random.step Random.bool >> Tuple.second) startingSeed
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
    | WithGeneratedId (String -> id)


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
                let
                    ( uuid, newStream ) =
                        UuidStream.consume state.idStream
                in
                    { state
                        | entities =
                            Dict.insert (toString (mkId uuid))
                                (Entity
                                    { id = mkId uuid
                                    , components = components
                                    }
                                )
                                state.entities
                        , idStream = newStream
                    }


{-| get an entity by id
-}
getEntity : id -> GameState id components -> Maybe (Entity id components)
getEntity id (GameState state) =
    Dict.get (toString id) state.entities


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
        { model : Game userModel id components
        , cmds : Cmd msg
        , dt : Time
        }


getUserModel : SystemRuntime userModel id components msg -> Game userModel id components
getUserModel (SystemRuntime { model }) =
    model


getGameState : SystemRuntime userModel id components msg -> GameState id components
getGameState (SystemRuntime { model }) =
    model.wyrmGameState


getDeltaTime : SystemRuntime userModel id components msg -> Time
getDeltaTime (SystemRuntime { dt }) =
    dt


mapUserModel :
    (Game userModel id components -> Game userModel id components)
    -> SystemRuntime userModel id components msg
    -> SystemRuntime userModel id components msg
mapUserModel f (SystemRuntime systemRuntime) =
    SystemRuntime { systemRuntime | model = f systemRuntime.model }


mapGameState : (GameState id components -> GameState id components) -> SystemRuntime userModel id components msg -> SystemRuntime userModel id components msg
mapGameState f (SystemRuntime systemRuntime) =
    let
        model =
            systemRuntime.model
    in
        SystemRuntime { systemRuntime | model = { model | wyrmGameState = f model.wyrmGameState } }


withGameState :
    (GameState id components -> ( value, GameState id components ))
    -> SystemRuntime userModel id components msg
    -> ( value, SystemRuntime userModel id components msg )
withGameState f (SystemRuntime systemRuntime) =
    let
        model =
            systemRuntime.model

        ( value, newState ) =
            f model.wyrmGameState
    in
        ( value, SystemRuntime { systemRuntime | model = { model | wyrmGameState = newState } } )


sendCmd : Cmd msg -> SystemRuntime userModel id components msg -> SystemRuntime userModel id components msg
sendCmd cmd (SystemRuntime systemRuntime) =
    --TODO: check on order of Cmd.batch
    SystemRuntime { systemRuntime | cmds = Cmd.batch [ cmd, systemRuntime.cmds ] }


type System userModel id components msg
    = System (SystemRuntime userModel id components msg -> SystemRuntime userModel id components msg)


runSystems : Time -> Game userModel id components -> List (System userModel id components msg) -> SystemRuntime userModel id components msg
runSystems dt userModel systems =
    List.foldl
        (\(System sysFn) systemRuntime ->
            sysFn systemRuntime
        )
        (SystemRuntime
            { model = userModel
            , dt = dt
            , cmds = Cmd.none
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
            (\id _ ( (SystemRuntime systemRuntime) as runtime, acc ) ->
                case Dict.get id (getEntities systemRuntime.model.wyrmGameState) of
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
            (getEntities startingRuntime.model.wyrmGameState)
            |> Tuple.first



------ COMPONENT ------
-- Updater helpers


type ComponentFocus components comp
    = ComponentFocus
        { get : components -> Maybe comp
        , set : Maybe comp -> components -> components
        }


createFocus : (components -> Maybe comp) -> (Maybe comp -> components -> components) -> ComponentFocus components comp
createFocus get set =
    ComponentFocus
        { get = get
        , set = set
        }


set : ComponentFocus components comp -> comp -> components -> components
set (ComponentFocus { set }) comp components =
    set (Just comp) components


update : ComponentFocus components comp -> (Maybe comp -> Maybe comp) -> components -> components
update (ComponentFocus { get, set }) f components =
    set (f (get components)) components


remove : ComponentFocus components comp -> components -> components
remove (ComponentFocus { set }) =
    set Nothing



-- Helpers for matching entities


matchEntity : Maybe (Entity id components) -> Maybe (Entity id components)
matchEntity =
    identity


with : ComponentFocus components comp -> Maybe (Entity id components) -> Maybe ( Entity id components, (comp -> r) -> r )
with (ComponentFocus { get }) maybeEntity =
    case maybeEntity of
        Nothing ->
            Nothing

        Just ((Entity { components }) as entity) ->
            Maybe.map ((,) entity << (|>)) (get components)


andWith : ComponentFocus components comp -> Maybe ( Entity id components, a -> comp -> r ) -> Maybe ( Entity id components, a -> r )
andWith (ComponentFocus { get }) =
    Maybe.andThen
        (\( (Entity { components }) as entity, cont ) ->
            Maybe.map (\component -> (,) entity <| \c -> (|>) component (cont c)) (get components)
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



------ INTERNALS ------


randomWith : Generator a -> GameState id components -> ( a, GameState id components )
randomWith generator (GameState gameState) =
    let
        ( seed, newStream ) =
            Stream.consume gameState.randomSeedStream

        ( value, _ ) =
            Random.step generator seed
    in
        (,)
            value
            (GameState
                { gameState | randomSeedStream = newStream }
            )


randomStreamWith : Generator a -> GameState id components -> ( Stream a, GameState id components )
randomStreamWith generator (GameState gameState) =
    let
        ( seed, newStream ) =
            Stream.consume gameState.randomSeedStream

        ( independentSeed, _ ) =
            Random.step Random.independentSeed seed

        mkNewStream seed gen =
            let
                ( value, newSeed ) =
                    Random.step gen seed
            in
                Stream.stream value (\() -> mkNewStream newSeed gen)
    in
        (,)
            (mkNewStream independentSeed generator)
            (GameState
                { gameState
                    | randomSeedStream = newStream
                }
            )
