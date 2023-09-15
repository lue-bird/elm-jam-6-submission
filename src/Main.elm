port module Main exposing (main)

{-| How the dragging works (copied from <https://github.com/w0rm/elm-physics/blob/main/examples/src/Lack.elm>):

1.  Uses `World.raycast` on mouse down to pick a body
2.  Creates a temporary body at the mouse position
3.  Connects the temporary body with the selected body using a point to point constraint
4.  Moves the temporary body on mouse move
5.  Removes the temporary body on mouse up

-}

import Acceleration
import Angle exposing (Angle)
import Audio exposing (AudioData)
import Axis2d exposing (Axis2d)
import Axis3d exposing (Axis3d)
import Block3d exposing (Block3d)
import Browser
import Browser.Dom
import Browser.Events
import Camera3d exposing (Camera3d)
import Circle2d exposing (Circle2d)
import Color exposing (Color)
import Dict exposing (Dict)
import Direction2d exposing (Direction2d)
import Direction3d
import Duration exposing (Duration, Seconds, seconds)
import Element as Ui
import Element.Background as UiBackground
import Element.Border as UiBorder
import Element.Font as UiFont
import Element.Input as UiInput
import Frame2d exposing (Frame2d)
import Frame3d
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Json.Decode exposing (Decoder)
import Json.Encode
import Key
import Length exposing (Length, Meters, millimeters)
import LineSegment2d exposing (LineSegment2d)
import List.Extra
import Mass exposing (kilograms)
import Physics.Body exposing (Body)
import Physics.Constraint as Constraint
import Physics.Coordinates exposing (BodyCoordinates, WorldCoordinates)
import Physics.Shape
import Physics.World exposing (World)
import Pixels exposing (Pixels, pixels)
import Plane3d
import Point2d exposing (Point2d)
import Point3d exposing (Point3d)
import Quantity exposing (Quantity, Rate)
import Random
import Random.Extra
import Reaction exposing (Reaction)
import RecordWithoutConstructorFunction exposing (RecordWithoutConstructorFunction)
import Rectangle2d
import Scene3d exposing (Entity)
import Scene3d.Material as Material
import Scene3d.Mesh
import Sphere3d
import Task
import Time
import Tree exposing (Tree)
import Tree.Path exposing (TreePath)
import Triangle3d
import Vector2d exposing (Vector2d)
import Vector3d
import Viewpoint3d
import VirtualDom


type Event
    = AudioLoaded { piece : AudioKind, result : Result Audio.LoadError Audio.Source }
    | GameWindowSized { width : Float, height : Float }
    | InitialRandomSeedReceived Random.Seed
    | InitialTimeReceived Time.Posix
    | FrameTickPassed Time.Posix
    | KeyPressed Key.Key
    | KeyReleased Key.Key
    | MousePressed (Axis3d Meters WorldCoordinates)
    | MouseMoved (Axis3d Meters WorldCoordinates)
    | MouseReleased


type alias State =
    RecordWithoutConstructorFunction
        { audio : EachAudio (Result Audio.LoadError Audio.Source)
        , windowSize : { width : Float, height : Float }
        , audioTimes : EachAudio (List Time.Posix)
        , keysPressed : List Key.Key
        , randomSeed : Random.Seed
        , lastTick : Time.Posix
        , initialTime : Time.Posix
        , world : World BodyKind
        , camera : Camera3d Meters WorldCoordinates
        , maybeRaycastResult : Maybe (Physics.World.RaycastResult BodyKind)
        , playerPast : PlayerPast
        }


type PlayerPast
    = PlayerPastFrozen (List (Point3d Meters WorldCoordinates))
    | PlayerLeavingTrail (List (Point3d Meters WorldCoordinates))


type Effect
    = LoadAudio AudioKind
    | RequestInitialRandomSeed
    | RequestInitialTime
    | GameRequestInitialWindowSize


main : Program () (Audio.Model Event State) (Audio.Msg Event)
main =
    Audio.documentWithAudio
        { init =
            init >> Reaction.toTuple3 interpretEffect
        , update =
            \_ event ->
                reactTo event >> Reaction.toTuple3 interpretEffect
        , subscriptions =
            \_ -> subscriptions
        , view =
            \_ -> uiDocument
        , audio = audio
        , audioPort =
            { toJS = audioPortToJS
            , fromJS = audioPortFromJS
            }
        }


type AudioKind
    = AudioRoomChange
    | AudioMusic


audioKinds : List AudioKind
audioKinds =
    [ AudioRoomChange, AudioMusic ]


type alias EachAudio perKind =
    { roomChange : perKind
    , music : perKind
    }


eachAudio : perKind -> EachAudio perKind
eachAudio perKind =
    { roomChange = perKind
    , music = perKind
    }


alterAudioOfKind : AudioKind -> (a -> a) -> EachAudio a -> EachAudio a
alterAudioOfKind kind f =
    case kind of
        AudioRoomChange ->
            \r -> { r | roomChange = r.roomChange |> f }

        AudioMusic ->
            \r -> { r | music = r.music |> f }


accessAudioOfKind : AudioKind -> EachAudio a -> a
accessAudioOfKind kind =
    case kind of
        AudioRoomChange ->
            .roomChange

        AudioMusic ->
            .music


audioPieceToName : AudioKind -> String
audioPieceToName =
    \audioPiece ->
        case audioPiece of
            AudioRoomChange ->
                "room-change"

            AudioMusic ->
                "music"


init : () -> Reaction State Effect
init () =
    Reaction.to
        { audio = eachAudio (Err Audio.UnknownError)
        , windowSize =
            -- dummy
            { width = 0, height = 0 }
        , audioTimes = eachAudio []
        , world = initialWorld
        , camera =
            Camera3d.perspective
                { viewpoint =
                    Viewpoint3d.lookAt
                        { eyePoint = Point3d.meters 0 0 cameraHeight
                        , focalPoint = Point3d.origin
                        , upDirection = Direction3d.positiveZ
                        }
                , verticalFieldOfView = Angle.degrees 60
                }
        , maybeRaycastResult = Nothing
        , keysPressed = []
        , randomSeed =
            -- dummy
            Random.initialSeed 1635127483
        , initialTime =
            -- dummy
            Time.millisToPosix -1
        , lastTick =
            -- dummy
            Time.millisToPosix -1
        , playerPast = PlayerLeavingTrail []
        }
        |> Reaction.effectsAdd
            [ RequestInitialRandomSeed
            , RequestInitialTime
            , GameRequestInitialWindowSize
            ]
        |> Reaction.effectsAdd
            (audioKinds |> List.map LoadAudio)


cameraHeight =
    1.05



-- 55


reactTo : Event -> (State -> Reaction State Effect)
reactTo event =
    case event of
        AudioLoaded audioLoaded ->
            \state ->
                Reaction.to
                    { state
                        | audio =
                            state.audio
                                |> alterAudioOfKind audioLoaded.piece (\_ -> audioLoaded.result)
                    }

        GameWindowSized size ->
            \state -> Reaction.to { state | windowSize = size }

        InitialRandomSeedReceived initialRandomSeed ->
            \state ->
                Reaction.to
                    { state | randomSeed = initialRandomSeed }

        InitialTimeReceived initialTime ->
            \state ->
                Reaction.to
                    { state
                        | initialTime = initialTime
                        , lastTick = initialTime
                    }

        FrameTickPassed newSimulationTime ->
            \state ->
                let
                    sinceLastTick =
                        Duration.from state.lastTick newSimulationTime

                    withNewLastTick =
                        { state
                            | lastTick = newSimulationTime
                        }

                    newSimulatedPhysicsWorld =
                        Physics.World.simulate sinceLastTick state.world
                in
                case
                    newSimulatedPhysicsWorld
                        |> Physics.World.bodies
                        |> List.filterMap
                            (\body ->
                                case body |> Physics.Body.data of
                                    Player ->
                                        Just body

                                    _ ->
                                        Nothing
                            )
                of
                    -- impossible
                    [] ->
                        Reaction.to withNewLastTick

                    playerBody :: _ ->
                        let
                            ( playerX, playerY, _ ) =
                                playerBody |> Physics.Body.originPoint |> Point3d.toTuple Length.inMeters

                            roomCenter =
                                { x = playerX |> round |> toFloat
                                , y = (playerY / ratioWidthToHeight |> round |> toFloat) * ratioWidthToHeight
                                }

                            newCameraHeight =
                                case withNewLastTick.playerPast of
                                    PlayerLeavingTrail _ ->
                                        cameraHeight

                                    PlayerPastFrozen _ ->
                                        ((withNewLastTick.camera |> Camera3d.viewpoint |> Viewpoint3d.eyePoint |> Point3d.zCoordinate |> Length.inMeters)
                                            + 0.001
                                        )
                                            * 1.0015

                            cameraInNewRoom =
                                Camera3d.perspective
                                    { viewpoint =
                                        Viewpoint3d.lookAt
                                            { eyePoint =
                                                Point3d.meters
                                                    -- snap to rooms
                                                    roomCenter.x
                                                    roomCenter.y
                                                    newCameraHeight
                                            , focalPoint = Point3d.meters roomCenter.x roomCenter.y 0
                                            , upDirection = Direction3d.positiveZ
                                            }
                                    , verticalFieldOfView = Angle.degrees 60
                                    }

                            withCameraInNewRoom =
                                { withNewLastTick
                                    | camera = cameraInNewRoom
                                    , playerPast =
                                        if playerX >= 16.94 then
                                            case withNewLastTick.playerPast of
                                                PlayerLeavingTrail trail ->
                                                    PlayerPastFrozen (trail |> List.reverse)

                                                PlayerPastFrozen _ ->
                                                    withNewLastTick.playerPast

                                        else
                                            case withNewLastTick.playerPast of
                                                PlayerLeavingTrail trail ->
                                                    if playerBody |> Physics.Body.velocity |> Vector3d.for Duration.second |> Vector3d.length |> Quantity.lessThan (Length.meters 0.0001) then
                                                        withNewLastTick.playerPast

                                                    else
                                                        PlayerLeavingTrail
                                                            ((playerBody |> Physics.Body.originPoint) :: trail)

                                                PlayerPastFrozen [] ->
                                                    PlayerPastFrozen []

                                                PlayerPastFrozen (_ :: trailAfterPosition) ->
                                                    PlayerPastFrozen trailAfterPosition
                                }

                            newPhysicsWorld =
                                newSimulatedPhysicsWorld
                                    |> Physics.World.update
                                        (\body ->
                                            case body |> Physics.Body.data of
                                                MouseImitation mouseImitation ->
                                                    if playerX >= ((body |> Physics.Body.originPoint |> Point3d.xCoordinate |> Length.inMeters) - 1) && playerX < 16.5 then
                                                        let
                                                            toPlayerX =
                                                                Vector3d.from
                                                                    (body |> Physics.Body.originPoint)
                                                                    (Point3d.meters
                                                                        playerX
                                                                        (body |> Physics.Body.originPoint |> Point3d.yCoordinate |> Length.inMeters)
                                                                        0
                                                                    )
                                                        in
                                                        body
                                                            |> Physics.Body.applyForce
                                                                (Quantity.times
                                                                    ((toPlayerX |> Vector3d.length |> Length.inMeters) |> Acceleration.metersPerSecondSquared)
                                                                    (Mass.kilograms 0.2)
                                                                )
                                                                (toPlayerX |> Vector3d.direction |> Maybe.withDefault Direction3d.positiveX)
                                                                (body |> Physics.Body.originPoint)

                                                    else
                                                        body

                                                PlayerPast ->
                                                    case withNewLastTick.playerPast of
                                                        PlayerLeavingTrail _ ->
                                                            body

                                                        PlayerPastFrozen [] ->
                                                            body

                                                        PlayerPastFrozen (position :: trailAfterPosition) ->
                                                            body
                                                                |> Physics.Body.moveTo
                                                                    (position
                                                                        |> Point3d.translateBy (Vector3d.meters 17 -(1.5 * ratioWidthToHeight) 0)
                                                                    )
                                                                |> Physics.Body.rotateAround Axis3d.z (Angle.turns 0.01)

                                                _ ->
                                                    body
                                        )

                            ( oldCameraX, oldCameraY, _ ) =
                                state.camera |> Camera3d.viewpoint |> Viewpoint3d.eyePoint |> Point3d.toTuple Length.inMeters
                        in
                        if oldCameraX /= roomCenter.x || oldCameraY /= roomCenter.y then
                            Reaction.to
                                { withCameraInNewRoom
                                    | world =
                                        newPhysicsWorld
                                            |> Physics.World.keepIf
                                                (\body -> Physics.Body.data body /= Mouse)
                                    , audioTimes =
                                        withCameraInNewRoom.audioTimes
                                            |> (\r -> { r | roomChange = r.roomChange |> (::) newSimulationTime })
                                }

                        else if
                            Vector3d.length (playerBody |> Physics.Body.velocity |> Vector3d.for Duration.second)
                                |> Quantity.greaterThan (Length.meters 1.4)
                        then
                            Reaction.to
                                { withCameraInNewRoom
                                    | world =
                                        newPhysicsWorld
                                            |> Physics.World.keepIf (\body -> (body |> Physics.Body.data) /= Mouse)
                                            |> Physics.World.update
                                                (\body ->
                                                    if body == playerBody then
                                                        body |> Physics.Body.withDamping { angular = 0.2, linear = 0.5 }

                                                    else
                                                        body |> Physics.Body.withDamping playerDefaultDamping
                                                )
                                }

                        else
                            Reaction.to
                                { withCameraInNewRoom
                                    | world = newPhysicsWorld
                                }

        KeyPressed key ->
            \state ->
                Reaction.to
                    { state | keysPressed = state.keysPressed |> (::) key }

        KeyReleased key ->
            \state ->
                Reaction.to
                    { state
                        | keysPressed =
                            state.keysPressed |> List.filter (\keyPressed -> keyPressed /= key)
                    }

        MousePressed mouseRay ->
            \model ->
                case Physics.World.raycast mouseRay model.world of
                    Just raycastResult ->
                        if raycastResult.body |> Physics.Body.data |> isDraggable then
                            let
                                worldPoint =
                                    Point3d.placeIn
                                        (Physics.Body.frame raycastResult.body)
                                        raycastResult.point

                                mouse =
                                    Physics.Body.compound [] Mouse
                                        |> Physics.Body.moveTo worldPoint
                            in
                            Reaction.to
                                { model
                                    | maybeRaycastResult = Just raycastResult
                                    , world =
                                        model.world
                                            |> Physics.World.add mouse
                                            |> Physics.World.constrain
                                                (\b1 b2 ->
                                                    case Physics.Body.data b1 of
                                                        Mouse ->
                                                            if b2 == raycastResult.body then
                                                                [ Constraint.pointToPoint
                                                                    Point3d.origin
                                                                    raycastResult.point
                                                                ]

                                                            else
                                                                []

                                                        _ ->
                                                            []
                                                )
                                }

                        else
                            Reaction.to model

                    Nothing ->
                        Reaction.to model

        MouseMoved mouseRay ->
            \state ->
                case state.maybeRaycastResult of
                    Just raycastResult ->
                        let
                            worldPoint =
                                Point3d.placeIn
                                    (Physics.Body.frame raycastResult.body)
                                    raycastResult.point

                            plane =
                                Plane3d.through
                                    worldPoint
                                    (Viewpoint3d.viewDirection (Camera3d.viewpoint state.camera))
                        in
                        Reaction.to
                            { state
                                | world =
                                    Physics.World.update
                                        (\body ->
                                            if Physics.Body.data body == Mouse then
                                                case Axis3d.intersectionWithPlane plane mouseRay of
                                                    Just intersection ->
                                                        Physics.Body.moveTo intersection body

                                                    Nothing ->
                                                        body

                                            else
                                                body
                                        )
                                        state.world
                            }

                    Nothing ->
                        Reaction.to state

        MouseReleased ->
            \state ->
                Reaction.to
                    { state
                        | maybeRaycastResult = Nothing
                        , world =
                            state.world
                                |> Physics.World.keepIf
                                    (\body -> Physics.Body.data body /= Mouse)
                    }


subscriptions : State -> Sub Event
subscriptions =
    \state ->
        [ Browser.Events.onResize
            (\width height ->
                { width = width |> toFloat, height = height |> toFloat }
                    |> GameWindowSized
            )
        , Browser.Events.onAnimationFrame FrameTickPassed
        , Browser.Events.onKeyDown (Json.Decode.map KeyPressed Key.decoder)
        , Browser.Events.onKeyUp (Json.Decode.map KeyReleased Key.decoder)
        ]
            |> Sub.batch


port audioPortToJS : Json.Encode.Value -> Cmd msg_


port audioPortFromJS : (Json.Decode.Value -> msg) -> Sub msg


interpretEffect : Effect -> Reaction.EffectInterpretation Event
interpretEffect =
    \effect ->
        case effect of
            LoadAudio piece ->
                Reaction.audioCommands
                    [ Audio.loadAudio
                        (\result -> AudioLoaded { result = result, piece = piece })
                        ([ "public/", piece |> audioPieceToName, ".mp3" ] |> String.concat)
                    ]

            RequestInitialRandomSeed ->
                Reaction.commands [ Random.independentSeed |> Random.generate InitialRandomSeedReceived ]

            RequestInitialTime ->
                Reaction.commands [ Time.now |> Task.perform InitialTimeReceived ]

            GameRequestInitialWindowSize ->
                Reaction.commands
                    [ Browser.Dom.getViewport
                        |> Task.perform
                            (\viewport ->
                                { width = viewport.viewport.width
                                , height = viewport.viewport.height
                                }
                                    |> GameWindowSized
                            )
                    ]


type BodyKind
    = Mouse
    | Player
    | Floor
    | BlockingImmovableWall { length : Length }
    | DraggableBlock { length : Length }
    | DraggableBallWithCubeBehavior { radius : Length }
    | MouseImitation { id : Maybe Int }
    | PlayerImitation { id : Maybe Int }
    | PlayerPast


isDraggable : BodyKind -> Bool
isDraggable kind =
    case kind of
        Player ->
            True

        DraggableBlock _ ->
            True

        DraggableBallWithCubeBehavior _ ->
            True

        PlayerImitation _ ->
            True

        PlayerPast ->
            False

        BlockingImmovableWall _ ->
            False

        Mouse ->
            False

        MouseImitation _ ->
            False

        Floor ->
            False


worldAddBodies additionalBodies world =
    List.foldl
        (\body worldSoFar -> worldSoFar |> Physics.World.add body)
        world
        additionalBodies


connectedWithMouseImitations =
    [ \id ->
        ( mouseImitationAtXYWithData 14.6 -(0.7 * ratioWidthToHeight) { id = id }
        , playerImitationWith { id = id }
            |> Physics.Body.moveTo (Point3d.meters 13.9 -(1.1 * ratioWidthToHeight) 0)
        )
    , \id ->
        ( mouseImitationAtXYWithData 14.5 -(1.4 * ratioWidthToHeight) { id = id }
        , playerImitationWith { id = id }
            |> Physics.Body.moveTo (Point3d.meters 13.6 -(1.4 * ratioWidthToHeight) 0)
        )
    , \id ->
        ( mouseImitationAtXYWithData 14.05 -(0.6 * ratioWidthToHeight) { id = id }
        , playerImitationWith { id = id }
            |> Physics.Body.moveTo (Point3d.meters 13.75 -(0.9 * ratioWidthToHeight) 0)
        )
    , \id ->
        ( mouseImitationAtXYWithData 13.7 -(1.1 * ratioWidthToHeight) { id = id }
        , playerImitationWith { id = id }
            |> Physics.Body.moveTo (Point3d.meters 13.9 -(1.3 * ratioWidthToHeight) 0)
        )
    , \id ->
        ( mouseImitationAtXYWithData 12.6 -(0.7 * ratioWidthToHeight) { id = id }
        , playerImitationWith { id = id }
            |> Physics.Body.moveTo (Point3d.meters 12.9 -(1.1 * ratioWidthToHeight) 0)
        )
    , \id ->
        ( mouseImitationAtXYWithData 13 -(1.4 * ratioWidthToHeight) { id = id }
        , playerImitationWith { id = id }
            |> Physics.Body.moveTo (Point3d.meters 12.6 -(1.4 * ratioWidthToHeight) 0)
        )
    , \id ->
        ( mouseImitationAtXYWithData 13.05 -(0.4 * ratioWidthToHeight) { id = id }
        , playerImitationWith { id = id }
            |> Physics.Body.moveTo (Point3d.meters 12.75 -(0.9 * ratioWidthToHeight) 0)
        )
    , \id ->
        ( mouseImitationAtXYWithData 13.3 -(1.5 * ratioWidthToHeight) { id = id }
        , playerImitationWith { id = id }
            |> Physics.Body.moveTo (Point3d.meters 12.9 -(1.3 * ratioWidthToHeight) 0)
        )
    , \id ->
        ( mouseImitationAtXYWithData 11.6 -(0.7 * ratioWidthToHeight) { id = id }
        , playerImitationWith { id = id }
            |> Physics.Body.moveTo (Point3d.meters 11.9 -(1.1 * ratioWidthToHeight) 0)
        )
    , \id ->
        ( mouseImitationAtXYWithData 12.6 -(0.2 * ratioWidthToHeight) { id = id }
        , playerImitationWith { id = id }
            |> Physics.Body.moveTo (Point3d.meters 12.9 -(1.1 * ratioWidthToHeight) 0)
        )
    , \id ->
        ( mouseImitationAtXYWithData 14.3 -(1.4 * ratioWidthToHeight) { id = id }
        , playerImitationWith { id = id }
            |> Physics.Body.moveTo (Point3d.meters 14 -(1.4 * ratioWidthToHeight) 0)
        )
    , \id ->
        ( mouseImitationAtXYWithData 12.05 -(0.4 * ratioWidthToHeight) { id = id }
        , playerImitationWith { id = id }
            |> Physics.Body.moveTo (Point3d.meters 11.75 -(0.9 * ratioWidthToHeight) 0)
        )
    , \id ->
        ( mouseImitationAtXYWithData 13.3 -(1.5 * ratioWidthToHeight) { id = id }
        , playerImitationWith { id = id }
            |> Physics.Body.moveTo (Point3d.meters 12.9 -(1.3 * ratioWidthToHeight) 0)
        )
    ]
        |> List.indexedMap (\i f -> f (Just i))


initialWorld : World BodyKind
initialWorld =
    Physics.World.empty
        |> Physics.World.withGravity (Acceleration.gees 1) Direction3d.negativeZ
        |> Physics.World.add
            (blockingImmovableWallBody { length = Length.meters ratioWidthToHeight }
                |> Physics.Body.rotateAround Axis3d.z (Angle.turns (1 / 4))
                |> Physics.Body.moveTo (Point3d.meters -(1 / 2) 0 0)
            )
        |> worldAddBodies mainRoute
        |> worldAddBodies (connectedWithMouseImitations |> List.map Tuple.first)
        |> worldAddBodies (connectedWithMouseImitations |> List.map Tuple.second)
        |> Physics.World.constrain
            (\body0 body1 ->
                case ( body0 |> Physics.Body.data, body1 |> Physics.Body.data ) of
                    ( MouseImitation mouseImitationData, PlayerImitation playerImitationData ) ->
                        case ( mouseImitationData.id, playerImitationData.id ) of
                            ( Just mouseId, Just playerId ) ->
                                if mouseId == playerId then
                                    [ Constraint.distance (Length.meters 0.25)
                                    ]

                                else
                                    []

                            _ ->
                                []

                    _ ->
                        []
            )
        |> worldAddBodies (mainRoute |> List.map (Physics.Body.translateBy (Vector3d.meters 17 -(1 * ratioWidthToHeight) 0)))
        |> Physics.World.add (Physics.Body.plane Floor)
        |> Physics.World.add
            (Physics.Body.compound [ Physics.Shape.sphere (Sphere3d.atOrigin (Length.meters 0.15)) ] Player
                |> Physics.Body.withBehavior (Physics.Body.dynamic (kilograms 1))
                |> Physics.Body.withDamping playerDefaultDamping
                -- |> Physics.Body.moveTo (Point3d.fromMeters { x = 14.2, y = -1.2, z = 0.22 })
                |> Physics.Body.moveTo (Point3d.fromMeters { x = 0, y = 0, z = 0.22 })
            )
        |> Physics.World.add
            (Physics.Body.compound [] PlayerPast
                |> Physics.Body.withBehavior (Physics.Body.dynamic (Mass.kilograms 20))
                |> Physics.Body.withDamping playerDefaultDamping
                |> Physics.Body.moveTo (Point3d.fromMeters { x = 17, y = -1 * ratioWidthToHeight, z = 0.22 })
            )


mainRoute =
    [ blockingImmovableWallBody { length = Length.meters 1 }
        |> Physics.Body.moveTo (Point3d.meters 0 -(ratioWidthToHeight / 2) 0)
    , blockingImmovableWallBody { length = Length.meters 1 }
        |> Physics.Body.moveTo (Point3d.meters 0 (ratioWidthToHeight / 2) 0)
    , blockingImmovableWallBody { length = Length.meters 1 }
        |> Physics.Body.moveTo (Point3d.meters 1 -(ratioWidthToHeight / 2) 0)
    , blockingImmovableWallBody { length = Length.meters 1 }
        |> Physics.Body.moveTo (Point3d.meters 1 (ratioWidthToHeight / 2) 0)
    , blockingImmovableWallBody { length = Length.meters (ratioWidthToHeight / 2) }
        |> Physics.Body.rotateAround Axis3d.z (Angle.turns -0.3)
        |> Physics.Body.moveTo (Point3d.meters 0.9 (ratioWidthToHeight / 4) 0)
    , blockingImmovableWallBody { length = Length.meters (ratioWidthToHeight / 2) }
        |> Physics.Body.rotateAround Axis3d.z (Angle.turns 0.3)
        |> Physics.Body.moveTo (Point3d.meters 1.3 -(ratioWidthToHeight / 4) 0)
    , blockingImmovableWallBody { length = Length.meters 1 }
        |> Physics.Body.moveTo (Point3d.meters 2 -(ratioWidthToHeight / 2) 0)
    , blockingImmovableWallBody { length = Length.meters 1 }
        |> Physics.Body.moveTo (Point3d.meters 2 (ratioWidthToHeight / 2) 0)
    , draggableBlockBody { length = Length.meters (ratioWidthToHeight / 2) }
        |> Physics.Body.rotateAround Axis3d.z (Angle.turns 0.3)
        |> Physics.Body.moveTo (Point3d.meters 2.55 -(ratioWidthToHeight / 2) 0)
    , draggableBlockBody { length = Length.meters (ratioWidthToHeight / 2) }
        |> Physics.Body.rotateAround Axis3d.z (Angle.turns 0.2)
        |> Physics.Body.moveTo (Point3d.meters 2.54 (ratioWidthToHeight / 2) 0)
    , draggableBlockBody { length = Length.meters (ratioWidthToHeight / 4) }
        |> Physics.Body.rotateAround Axis3d.z (Angle.turns 0.2)
        |> Physics.Body.moveTo (Point3d.meters 2.7 0 0)
    , draggableBlockBody { length = Length.meters (ratioWidthToHeight / 4) }
        |> Physics.Body.rotateAround Axis3d.z (Angle.turns 0.3)
        |> Physics.Body.moveTo (Point3d.meters 2.7 -(ratioWidthToHeight / 4) 0)
    , draggableBlockBody { length = Length.meters (ratioWidthToHeight / 2) }
        |> Physics.Body.rotateAround Axis3d.z (Angle.turns 0.25)
        |> Physics.Body.moveTo (Point3d.meters 2.35 0 0)
    , draggableBlockBody { length = Length.meters (ratioWidthToHeight * 0.24) }
        |> Physics.Body.rotateAround Axis3d.z (Angle.turns 0.11)
        |> Physics.Body.moveTo (Point3d.meters 2.1 -(ratioWidthToHeight * 0.35) 0)
    , draggableBlockBody { length = Length.meters (ratioWidthToHeight * 0.24) }
        |> Physics.Body.rotateAround Axis3d.z (Angle.turns 0.37)
        |> Physics.Body.moveTo (Point3d.meters 2.1 (ratioWidthToHeight * 0.32) 0)
    , blockingImmovableWallBody { length = Length.meters 1 }
        |> Physics.Body.moveTo (Point3d.meters 3 -(1.5 * ratioWidthToHeight) 0)
    , blockingImmovableWallBody { length = Length.meters ratioWidthToHeight }
        |> Physics.Body.rotateAround Axis3d.z (Angle.turns 0.25)
        |> Physics.Body.moveTo (Point3d.meters 2.5 (-1 * ratioWidthToHeight) 0)
    , blockingImmovableWallBody { length = Length.meters ratioWidthToHeight }
        |> Physics.Body.rotateAround Axis3d.z (Angle.turns 0.25)
        |> Physics.Body.moveTo (Point3d.meters 3.5 (-1 * ratioWidthToHeight) 0)
    , draggableBlockBody { length = Length.meters (ratioWidthToHeight / 10) }
        |> Physics.Body.moveTo (Point3d.meters 3.25 -(0.7 * ratioWidthToHeight) 0)
    , draggableBlockBody { length = Length.meters (ratioWidthToHeight / 10) }
        |> Physics.Body.moveTo (Point3d.meters 3.25 -(0.9 * ratioWidthToHeight) 0)
    , draggableBlockBody { length = Length.meters (ratioWidthToHeight / 10) }
        |> Physics.Body.moveTo (Point3d.meters 3.25 -(1.1 * ratioWidthToHeight) 0)
    , draggableBlockBody { length = Length.meters (ratioWidthToHeight / 10) }
        |> Physics.Body.moveTo (Point3d.meters 3.25 -(1.3 * ratioWidthToHeight) 0)
    , blockingImmovableWallBody { length = Length.meters 1 }
        |> Physics.Body.moveTo (Point3d.meters 3 (1.5 * ratioWidthToHeight) 0)
    , blockingImmovableWallBody { length = Length.meters ratioWidthToHeight }
        |> Physics.Body.rotateAround Axis3d.z (Angle.turns 0.25)
        |> Physics.Body.moveTo (Point3d.meters 2.5 (1 * ratioWidthToHeight) 0)
    , blockingImmovableWallBody { length = Length.meters ratioWidthToHeight }
        |> Physics.Body.rotateAround Axis3d.z (Angle.turns 0.25)
        |> Physics.Body.moveTo (Point3d.meters 3.5 (1 * ratioWidthToHeight) 0)
    , blockingImmovableWallBody { length = Length.meters (ratioWidthToHeight / 4) }
        |> Physics.Body.moveTo (Point3d.meters 3.25 (0.9 * ratioWidthToHeight) 0)
    , blockingImmovableWallBody { length = Length.meters (ratioWidthToHeight / 4) }
        |> Physics.Body.moveTo (Point3d.meters 2.75 (1.2 * ratioWidthToHeight) 0)
    , blockingImmovableWallBody { length = Length.meters 1 }
        |> Physics.Body.moveTo (Point3d.meters 4 -(ratioWidthToHeight / 2) 0)
    , blockingImmovableWallBody { length = Length.meters 1 }
        |> Physics.Body.moveTo (Point3d.meters 4 (ratioWidthToHeight / 2) 0)
    , blockingImmovableWallBody { length = Length.meters 1 }
        |> Physics.Body.moveTo (Point3d.meters 5 (ratioWidthToHeight / 2) 0)
    , blockingImmovableWallBody { length = Length.meters 1 }
        |> Physics.Body.rotateAround Axis3d.z (Angle.turns 0.11)
        |> Physics.Body.moveTo (Point3d.meters 4.92 -(0.345 * ratioWidthToHeight) 0)
    , blockingImmovableWallBody { length = Length.meters 1 }
        |> Physics.Body.rotateAround Axis3d.z (Angle.turns 0.39)
        |> Physics.Body.moveTo (Point3d.meters 5 (0.35 * ratioWidthToHeight) 0)
    , blockingImmovableWallBody { length = Length.meters 1 }
        |> Physics.Body.moveTo (Point3d.meters 6 -(ratioWidthToHeight / 2) 0)
    , blockingImmovableWallBody { length = Length.meters 1 }
        |> Physics.Body.moveTo (Point3d.meters 6 (ratioWidthToHeight / 2) 0)
    , blockingImmovableWallBody { length = Length.meters ratioWidthToHeight }
        |> Physics.Body.rotateAround Axis3d.z (Angle.turns 0.25)
        |> Physics.Body.moveTo (Point3d.meters 6.5 0 0)
    , blockingImmovableWallBody { length = Length.meters 1 }
        |> Physics.Body.moveTo (Point3d.meters 6 -(0.5 * ratioWidthToHeight) 0)
    , blockingImmovableWallBody { length = Length.meters 1 }
        |> Physics.Body.moveTo (Point3d.meters 6 -(1.5 * ratioWidthToHeight) -0.298)
    , blockingImmovableWallBody { length = Length.meters 1 }
        |> Physics.Body.moveTo (Point3d.meters 5 -(1.5 * ratioWidthToHeight) -0.298)
    , blockingImmovableWallBody { length = Length.meters ratioWidthToHeight }
        |> Physics.Body.rotateAround Axis3d.z (Angle.turns 0.25)
        |> Physics.Body.moveTo (Point3d.meters 4.5 -ratioWidthToHeight 0)
    , blockingImmovableWallBody { length = Length.meters 0.1 }
        |> Physics.Body.moveTo (Point3d.meters 5.4 -(0.5 * ratioWidthToHeight) 0)
    , draggableBallWithCubeBehavior { radius = Length.meters 0.09 }
        |> Physics.Body.moveTo (Point3d.meters 5.4 -(0.5 * ratioWidthToHeight) 0.2)
    , draggableBallWithCubeBehavior { radius = Length.meters 0.09 }
        |> Physics.Body.moveTo (Point3d.meters 5.3 -(0.55 * ratioWidthToHeight) 0.2)
    , draggableBallWithCubeBehavior { radius = Length.meters 0.09 }
        |> Physics.Body.moveTo (Point3d.meters 5.2 -(0.6 * ratioWidthToHeight) 0.2)
    , draggableBallWithCubeBehavior { radius = Length.meters 0.1 }
        |> Physics.Body.moveTo (Point3d.meters 5.9 -(1.25 * ratioWidthToHeight) 0.3)
    , draggableBallWithCubeBehavior { radius = Length.meters 0.07 }
        |> Physics.Body.moveTo (Point3d.meters 5.6 -(1.35 * ratioWidthToHeight) 0.3)
    , draggableBallWithCubeBehavior { radius = Length.meters 0.1 }
        |> Physics.Body.moveTo (Point3d.meters 5.9 -(1 * ratioWidthToHeight) 0.3)
    , draggableBallWithCubeBehavior { radius = Length.meters 0.07 }
        |> Physics.Body.moveTo (Point3d.meters 6.1 -(0.9 * ratioWidthToHeight) 0.3)
    , draggableBallWithCubeBehavior { radius = Length.meters 0.07 }
        |> Physics.Body.moveTo (Point3d.meters 6.3 -(0.7 * ratioWidthToHeight) 0.3)
    , draggableBallWithCubeBehavior { radius = Length.meters 0.1 }
        |> Physics.Body.moveTo (Point3d.meters 5.9 -(1.75 * ratioWidthToHeight) 0.3)
    , draggableBallWithCubeBehavior { radius = Length.meters 0.07 }
        |> Physics.Body.moveTo (Point3d.meters 5.6 -(1.85 * ratioWidthToHeight) 0.3)
    , blockingImmovableWallBody { length = Length.meters 1 }
        |> Physics.Body.moveTo (Point3d.meters 6 -(2.5 * ratioWidthToHeight) 0)
    , blockingImmovableWallBody { length = Length.meters 1 }
        |> Physics.Body.moveTo (Point3d.meters 5 -(2.5 * ratioWidthToHeight) 0)
    , blockingImmovableWallBody { length = Length.meters ratioWidthToHeight }
        |> Physics.Body.rotateAround Axis3d.z (Angle.turns 0.25)
        |> Physics.Body.moveTo (Point3d.meters 6.5 -(2 * ratioWidthToHeight) 0)
    , blockingImmovableWallBody { length = Length.meters ratioWidthToHeight }
        |> Physics.Body.rotateAround Axis3d.z (Angle.turns 0.25)
        |> Physics.Body.moveTo (Point3d.meters 4.5 -(2 * ratioWidthToHeight) 0)
    , blockingImmovableWallBody { length = Length.meters (ratioWidthToHeight / 10) }
        |> Physics.Body.rotateAround Axis3d.z (Angle.turns 0.25)
        |> Physics.Body.moveTo (Point3d.meters 5.5 -(0.65 * ratioWidthToHeight) 0)
    , blockingImmovableWallBody { length = Length.meters (ratioWidthToHeight / 10) }
        |> Physics.Body.rotateAround Axis3d.z (Angle.turns 0.25)
        |> Physics.Body.moveTo (Point3d.meters 5.5 -(0.8 * ratioWidthToHeight) 0)
    , blockingImmovableWallBody { length = Length.meters (ratioWidthToHeight / 10) }
        |> Physics.Body.rotateAround Axis3d.z (Angle.turns 0.25)
        |> Physics.Body.moveTo (Point3d.meters 5.5 -(0.95 * ratioWidthToHeight) 0)
    , blockingImmovableWallBody { length = Length.meters (ratioWidthToHeight / 10) }
        |> Physics.Body.rotateAround Axis3d.z (Angle.turns 0.25)
        |> Physics.Body.moveTo (Point3d.meters 5.5 -(1.1 * ratioWidthToHeight) 0)
    , blockingImmovableWallBody { length = Length.meters (ratioWidthToHeight / 10) }
        |> Physics.Body.rotateAround Axis3d.z (Angle.turns 0.25)
        |> Physics.Body.moveTo (Point3d.meters 5.5 -(1.25 * ratioWidthToHeight) 0)
    , blockingImmovableWallBody { length = Length.meters (ratioWidthToHeight / 10) }
        |> Physics.Body.rotateAround Axis3d.z (Angle.turns 0.25)
        |> Physics.Body.moveTo (Point3d.meters 5.5 -(1.4 * ratioWidthToHeight) 0)
    , draggableBlockBody { length = Length.meters (ratioWidthToHeight / 10) }
        |> Physics.Body.moveTo (Point3d.meters 6.25 -(1.7 * ratioWidthToHeight) 0)
    , draggableBlockBody { length = Length.meters (ratioWidthToHeight / 10) }
        |> Physics.Body.moveTo (Point3d.meters 6.25 -(1.9 * ratioWidthToHeight) 0)
    , draggableBlockBody { length = Length.meters (ratioWidthToHeight / 10) }
        |> Physics.Body.moveTo (Point3d.meters 6.25 -(2.1 * ratioWidthToHeight) 0)
    , draggableBlockBody { length = Length.meters (ratioWidthToHeight / 10) }
        |> Physics.Body.moveTo (Point3d.meters 6.25 -(2.3 * ratioWidthToHeight) 0)
    , draggableBlockBody { length = Length.meters (ratioWidthToHeight / 10) }
        |> Physics.Body.moveTo (Point3d.meters 6 -(1.7 * ratioWidthToHeight) 0)
    , draggableBlockBody { length = Length.meters (ratioWidthToHeight / 10) }
        |> Physics.Body.moveTo (Point3d.meters 6 -(1.9 * ratioWidthToHeight) 0)
    , draggableBlockBody { length = Length.meters (ratioWidthToHeight / 10) }
        |> Physics.Body.moveTo (Point3d.meters 6 -(2.1 * ratioWidthToHeight) 0)
    , draggableBlockBody { length = Length.meters (ratioWidthToHeight / 10) }
        |> Physics.Body.moveTo (Point3d.meters 6 -(2.3 * ratioWidthToHeight) 0)
    , blockingImmovableWallBody { length = Length.meters 1 }
        |> Physics.Body.moveTo (Point3d.meters 7 -(0.5 * ratioWidthToHeight) 0)
    , blockingImmovableWallBody { length = Length.meters 1 }
        |> Physics.Body.moveTo (Point3d.meters 7 -(1.5 * ratioWidthToHeight) 0)
    , draggableBallWithCubeBehavior { radius = Length.meters 0.1 }
        |> Physics.Body.moveTo (Point3d.meters 6.9 -(1.25 * ratioWidthToHeight) 0.3)
    , draggableBallWithCubeBehavior { radius = Length.meters 0.07 }
        |> Physics.Body.moveTo (Point3d.meters 7.8 -(1.35 * ratioWidthToHeight) 0.3)
    , draggableBallWithCubeBehavior { radius = Length.meters 0.1 }
        |> Physics.Body.moveTo (Point3d.meters 7.9 -(1 * ratioWidthToHeight) 0.3)
    , draggableBallWithCubeBehavior { radius = Length.meters 0.07 }
        |> Physics.Body.moveTo (Point3d.meters 7.1 -(0.9 * ratioWidthToHeight) 0.3)
    , draggableBallWithCubeBehavior { radius = Length.meters 0.07 }
        |> Physics.Body.moveTo (Point3d.meters 6.3 -(0.7 * ratioWidthToHeight) 0.3)
    , blockingImmovableWallBody { length = Length.meters 1 }
        |> Physics.Body.moveTo (Point3d.meters 8 -(0.5 * ratioWidthToHeight) 0)
    , blockingImmovableWallBody { length = Length.meters 1 }
        |> Physics.Body.moveTo (Point3d.meters 8 -(1.5 * ratioWidthToHeight) 0)
    , draggableBallWithCubeBehavior { radius = Length.meters 0.11 }
        |> Physics.Body.moveTo (Point3d.meters 7.9 -(1.25 * ratioWidthToHeight) 0.3)
    , draggableBallWithCubeBehavior { radius = Length.meters 0.09 }
        |> Physics.Body.moveTo (Point3d.meters 8.8 -(1.35 * ratioWidthToHeight) 0.3)
    , draggableBallWithCubeBehavior { radius = Length.meters 0.05 }
        |> Physics.Body.moveTo (Point3d.meters 8.9 -(1 * ratioWidthToHeight) 0.3)
    , draggableBallWithCubeBehavior { radius = Length.meters 0.07 }
        |> Physics.Body.moveTo (Point3d.meters 8.1 -(0.86 * ratioWidthToHeight) 0.3)
    , draggableBallWithCubeBehavior { radius = Length.meters 0.18 }
        |> Physics.Body.moveTo (Point3d.meters 8.3 -(0.6 * ratioWidthToHeight) 0.3)
    , blockingImmovableWallBody { length = Length.meters 1 }
        |> Physics.Body.moveTo (Point3d.meters 9 -(0.5 * ratioWidthToHeight) 0)
    , blockingImmovableWallBody { length = Length.meters 1 }
        |> Physics.Body.moveTo (Point3d.meters 9 -(1.5 * ratioWidthToHeight) 0)
    , draggableBallWithCubeBehavior { radius = Length.meters 0.1 }
        |> Physics.Body.moveTo (Point3d.meters 8.9 -(1.25 * ratioWidthToHeight) 0.3)
    , draggableBallWithCubeBehavior { radius = Length.meters 0.07 }
        |> Physics.Body.moveTo (Point3d.meters 9.8 -(1.35 * ratioWidthToHeight) 0.3)
    , draggableBallWithCubeBehavior { radius = Length.meters 0.1 }
        |> Physics.Body.moveTo (Point3d.meters 9.9 -(1 * ratioWidthToHeight) 0.3)
    , draggableBallWithCubeBehavior { radius = Length.meters 0.07 }
        |> Physics.Body.moveTo (Point3d.meters 9.1 -(0.9 * ratioWidthToHeight) 0.3)
    , draggableBallWithCubeBehavior { radius = Length.meters 0.07 }
        |> Physics.Body.moveTo (Point3d.meters 10.3 -(0.7 * ratioWidthToHeight) 0.3)
    , blockingImmovableWallBody { length = Length.meters 1 }
        |> Physics.Body.moveTo (Point3d.meters 10 -(0.5 * ratioWidthToHeight) 0)
    , blockingImmovableWallBody { length = Length.meters 1 }
        |> Physics.Body.moveTo (Point3d.meters 10 -(1.5 * ratioWidthToHeight) 0)
    , draggableBallWithCubeBehavior { radius = Length.meters 0.11 }
        |> Physics.Body.moveTo (Point3d.meters 9.9 -(1.25 * ratioWidthToHeight) 0.3)
    , draggableBallWithCubeBehavior { radius = Length.meters 0.09 }
        |> Physics.Body.moveTo (Point3d.meters 10.8 -(1.35 * ratioWidthToHeight) 0.3)
    , draggableBallWithCubeBehavior { radius = Length.meters 0.05 }
        |> Physics.Body.moveTo (Point3d.meters 10.9 -(1 * ratioWidthToHeight) 0.3)
    , draggableBallWithCubeBehavior { radius = Length.meters 0.07 }
        |> Physics.Body.moveTo (Point3d.meters 10.1 -(0.86 * ratioWidthToHeight) 0.3)
    , draggableBallWithCubeBehavior { radius = Length.meters 0.18 }
        |> Physics.Body.moveTo (Point3d.meters 10.3 -(0.6 * ratioWidthToHeight) 0.3)
    , blockingImmovableWallBody { length = Length.meters 1 }
        |> Physics.Body.moveTo (Point3d.meters 11 -(0.5 * ratioWidthToHeight) 0)
    , blockingImmovableWallBody { length = Length.meters 1 }
        |> Physics.Body.moveTo (Point3d.meters 11 -(0.5 * ratioWidthToHeight) 0)
    , blockingImmovableWallBody { length = Length.meters 1 }
        |> Physics.Body.moveTo (Point3d.meters 11 -(1.5 * ratioWidthToHeight) 0)
    , blockingImmovableWallBody { length = Length.meters 1 }
        |> Physics.Body.moveTo (Point3d.meters 12 -(0.5 * ratioWidthToHeight) 0)
    , blockingImmovableWallBody { length = Length.meters 1 }
        |> Physics.Body.moveTo (Point3d.meters 12 -(1.5 * ratioWidthToHeight) 0)
    , playerImitation
        |> Physics.Body.moveTo (Point3d.meters 8.5 -(1 * ratioWidthToHeight) 0)
    , playerImitation
        |> Physics.Body.moveTo (Point3d.meters 9.8 -(0.7 * ratioWidthToHeight) 0)
    , playerImitation
        |> Physics.Body.moveTo (Point3d.meters 10.2 -(1.3 * ratioWidthToHeight) 0)
    , playerImitation
        |> Physics.Body.moveTo (Point3d.meters 10.6 -(1 * ratioWidthToHeight) 0)
    , playerImitation
        |> Physics.Body.moveTo (Point3d.meters 10.75 -(0.7 * ratioWidthToHeight) 0)
    , playerImitation
        |> Physics.Body.moveTo (Point3d.meters 10.9 -(1.3 * ratioWidthToHeight) 0)
    , playerImitation
        |> Physics.Body.moveTo (Point3d.meters 11 -(1.05 * ratioWidthToHeight) 0)
    , playerImitation
        |> Physics.Body.moveTo (Point3d.meters 11.15 -(0.6 * ratioWidthToHeight) 0)
    , playerImitation
        |> Physics.Body.moveTo (Point3d.meters 11.3 -(1.2 * ratioWidthToHeight) 0)
    , playerImitation
        |> Physics.Body.moveTo (Point3d.meters 11.2 -(1.3 * ratioWidthToHeight) 0)
    , playerImitation
        |> Physics.Body.moveTo (Point3d.meters 11.6 -(1 * ratioWidthToHeight) 0)
    , playerImitation
        |> Physics.Body.moveTo (Point3d.meters 11.75 -(0.7 * ratioWidthToHeight) 0)
    , playerImitation
        |> Physics.Body.moveTo (Point3d.meters 11.9 -(1.3 * ratioWidthToHeight) 0)
    , playerImitation
        |> Physics.Body.moveTo (Point3d.meters 12 -(1.05 * ratioWidthToHeight) 0)
    , playerImitation
        |> Physics.Body.moveTo (Point3d.meters 12.15 -(0.6 * ratioWidthToHeight) 0)
    , playerImitation
        |> Physics.Body.moveTo (Point3d.meters 12.3 -(1.2 * ratioWidthToHeight) 0)
    , playerImitation
        |> Physics.Body.moveTo (Point3d.meters 11.6 -(1 * ratioWidthToHeight) 0)
    , playerImitation
        |> Physics.Body.moveTo (Point3d.meters 11.75 -(0.7 * ratioWidthToHeight) 0)
    , playerImitation
        |> Physics.Body.moveTo (Point3d.meters 12.9 -(1.1 * ratioWidthToHeight) 0)
    , playerImitation
        |> Physics.Body.moveTo (Point3d.meters 12.6 -(1.4 * ratioWidthToHeight) 0)
    , playerImitation
        |> Physics.Body.moveTo (Point3d.meters 12.75 -(0.9 * ratioWidthToHeight) 0)
    , playerImitation
        |> Physics.Body.moveTo (Point3d.meters 12.9 -(1.3 * ratioWidthToHeight) 0)
    , blockingImmovableWallBody { length = Length.meters 1 }
        |> Physics.Body.moveTo (Point3d.meters 13 -(2.5 * ratioWidthToHeight) 0)
    , blockingImmovableWallBody { length = Length.meters 1 }
        |> Physics.Body.moveTo (Point3d.meters 13 (ratioWidthToHeight / 2) 0)
    , blockingImmovableWallBody { length = Length.meters 1 }
        |> Physics.Body.moveTo (Point3d.meters 14 -(2.5 * ratioWidthToHeight) 0)
    , blockingImmovableWallBody { length = Length.meters 1 }
        |> Physics.Body.moveTo (Point3d.meters 14 (ratioWidthToHeight / 2) 0)
    , blockingImmovableWallBody { length = Length.meters ratioWidthToHeight }
        |> Physics.Body.rotateAround Axis3d.z (Angle.turns 0.25)
        |> Physics.Body.moveTo (Point3d.meters 12.5 -(2 * ratioWidthToHeight) 0)
    , blockingImmovableWallBody { length = Length.meters ratioWidthToHeight }
        |> Physics.Body.rotateAround Axis3d.z (Angle.turns 0.25)
        |> Physics.Body.moveTo (Point3d.meters 12.5 (0 * ratioWidthToHeight) 0)
    , blockingImmovableWallBody { length = Length.meters ratioWidthToHeight }
        |> Physics.Body.rotateAround Axis3d.z (Angle.turns 0.25)
        |> Physics.Body.moveTo (Point3d.meters 14.5 -(2 * ratioWidthToHeight) 0)
    , blockingImmovableWallBody { length = Length.meters ratioWidthToHeight }
        |> Physics.Body.rotateAround Axis3d.z (Angle.turns 0.25)
        |> Physics.Body.moveTo (Point3d.meters 14.5 (0 * ratioWidthToHeight) 0)
    , mouseImitationAtXY 14.8 -(0.9 * ratioWidthToHeight)
    , mouseImitationAtXY 14.2 -(1.2 * ratioWidthToHeight)
    , mouseImitationAtXY 14 -(0.9 * ratioWidthToHeight)
    , mouseImitationAtXY 13.6 -(0.7 * ratioWidthToHeight)
    , draggableBallWithCubeBehavior { radius = Length.meters 0.11 }
        |> Physics.Body.moveTo (Point3d.meters 14.2 -(2.2 * ratioWidthToHeight) 0.3)
    , draggableBallWithCubeBehavior { radius = Length.meters 0.09 }
        |> Physics.Body.moveTo (Point3d.meters 14.2 (0.4 * ratioWidthToHeight) 0.3)
    , draggableBallWithCubeBehavior { radius = Length.meters 0.11 }
        |> Physics.Body.moveTo (Point3d.meters 13.9 -(2 * ratioWidthToHeight) 0.3)
    , draggableBallWithCubeBehavior { radius = Length.meters 0.09 }
        |> Physics.Body.moveTo (Point3d.meters 13.8 -(0 * ratioWidthToHeight) 0.3)
    , draggableBallWithCubeBehavior { radius = Length.meters 0.11 }
        |> Physics.Body.moveTo (Point3d.meters 13.2 -(2.4 * ratioWidthToHeight) 0.3)
    , draggableBallWithCubeBehavior { radius = Length.meters 0.09 }
        |> Physics.Body.moveTo (Point3d.meters 13.4 (0.4 * ratioWidthToHeight) 0.3)
    , draggableBallWithCubeBehavior { radius = Length.meters 0.11 }
        |> Physics.Body.moveTo (Point3d.meters 12.9 -(2 * ratioWidthToHeight) 0.3)
    , draggableBallWithCubeBehavior { radius = Length.meters 0.09 }
        |> Physics.Body.moveTo (Point3d.meters 12.8 -(0 * ratioWidthToHeight) 0.3)
    , draggableBallWithCubeBehavior { radius = Length.meters 0.11 }
        |> Physics.Body.moveTo (Point3d.meters 14.2 -(1.7 * ratioWidthToHeight) 0.3)
    , draggableBallWithCubeBehavior { radius = Length.meters 0.09 }
        |> Physics.Body.moveTo (Point3d.meters 14.2 -(0.1 * ratioWidthToHeight) 0.3)
    , draggableBallWithCubeBehavior { radius = Length.meters 0.11 }
        |> Physics.Body.moveTo (Point3d.meters 13.9 -(1.5 * ratioWidthToHeight) 0.3)
    , draggableBallWithCubeBehavior { radius = Length.meters 0.09 }
        |> Physics.Body.moveTo (Point3d.meters 13.8 -(0.5 * ratioWidthToHeight) 0.3)
    , draggableBallWithCubeBehavior { radius = Length.meters 0.11 }
        |> Physics.Body.moveTo (Point3d.meters 13.2 -(1.9 * ratioWidthToHeight) 0.3)
    , draggableBallWithCubeBehavior { radius = Length.meters 0.09 }
        |> Physics.Body.moveTo (Point3d.meters 13.4 -(0.1 * ratioWidthToHeight) 0.3)
    , draggableBallWithCubeBehavior { radius = Length.meters 0.11 }
        |> Physics.Body.moveTo (Point3d.meters 12.9 -(1.5 * ratioWidthToHeight) 0.3)
    , draggableBallWithCubeBehavior { radius = Length.meters 0.09 }
        |> Physics.Body.moveTo (Point3d.meters 12.8 -(0.5 * ratioWidthToHeight) 0.3)
    , blockingImmovableWallBody { length = Length.meters 1 }
        |> Physics.Body.moveTo (Point3d.meters 15 -(ratioWidthToHeight / 2) 0)
    , blockingImmovableWallBody { length = Length.meters 1 }
        |> Physics.Body.moveTo (Point3d.meters 15 -(1.5 * ratioWidthToHeight) 0)
    , blockingImmovableWallBody { length = Length.meters 1 }
        |> Physics.Body.moveTo (Point3d.meters 16 -(ratioWidthToHeight / 2) 0)
    , blockingImmovableWallBody { length = Length.meters 1 }
        |> Physics.Body.moveTo (Point3d.meters 16 -(1.5 * ratioWidthToHeight) 0)
    ]


mouseImitationAtXY x y =
    mouseImitationAtXYWithData x y { id = Nothing }


mouseImitationAtXYWithData x y data =
    Physics.Body.compound [ Physics.Shape.sphere (Sphere3d.atOrigin (Length.meters 0.02)) ]
        (MouseImitation data)
        |> Physics.Body.withBehavior (Physics.Body.dynamic (Mass.kilograms 1.4))
        |> Physics.Body.moveTo (Point3d.meters x y 0)


playerDefaultDamping =
    { angular = 0.15, linear = 0.1 }


blockingImmovableWallBody dimensions =
    Physics.Body.compound [ Physics.Shape.block (thickLinearBlock dimensions) ]
        (BlockingImmovableWall dimensions)


draggableBlockBody dimensions =
    Physics.Body.compound [ Physics.Shape.block (thickLinearBlock dimensions) ]
        (DraggableBlock dimensions)
        |> Physics.Body.withBehavior (Physics.Body.dynamic (kilograms 100))


draggableBallWithCubeBehavior data =
    let
        colliderSideLength =
            data.radius |> Quantity.multiplyBy 1.3
    in
    Physics.Body.block
        (Block3d.centeredOn
            Frame3d.atOrigin
            ( colliderSideLength, colliderSideLength, colliderSideLength )
        )
        (DraggableBallWithCubeBehavior data)
        |> Physics.Body.withBehavior (Physics.Body.dynamic (kilograms 1))


playerImitation =
    playerImitationWith { id = Nothing }


playerImitationWith data =
    Physics.Body.compound [ Physics.Shape.sphere (Sphere3d.atOrigin (Length.meters 0.15)) ] (PlayerImitation data)
        |> Physics.Body.withBehavior (Physics.Body.dynamic (kilograms 1))
        |> Physics.Body.withDamping playerDefaultDamping


tetrahedronWithRadius1 =
    let
        -- https://en.wikipedia.org/wiki/Tetrahedron
        p0 =
            Point3d.meters (sqrt (8 / 9)) 0 (-1 / 3)

        p1 =
            Point3d.meters -(sqrt (2 / 9)) (sqrt (2 / 3)) (-1 / 3)

        p2 =
            Point3d.meters -(sqrt (2 / 9)) -(sqrt (2 / 3)) (-1 / 3)

        p3 =
            Point3d.meters 0 0 1
    in
    Scene3d.Mesh.facets
        [ Triangle3d.from p0 p1 p2
        , Triangle3d.from p0 p1 p3
        , Triangle3d.from p0 p2 p3
        , Triangle3d.from p1 p2 p3
        ]


ratioWidthToHeight : Float
ratioWidthToHeight =
    16 / 9


ui : State -> Html Event
ui state =
    let
        adjustedSize =
            if state.windowSize.width < state.windowSize.height * ratioWidthToHeight then
                -- disproportional in height
                { width = state.windowSize.width
                , height = state.windowSize.width / ratioWidthToHeight
                }

            else
                -- might be disproportional in width
                { width = state.windowSize.height * ratioWidthToHeight
                , height = state.windowSize.height
                }

        ratioScreenHeightToSceneMeter =
            1

        rayTo point =
            Camera3d.ray
                state.camera
                (Rectangle2d.with
                    { x1 = pixels 0
                    , y1 = Pixels.float adjustedSize.height
                    , x2 = Pixels.float adjustedSize.width
                    , y2 = pixels 0
                    }
                )
                point
    in
    Html.div
        [ Html.Attributes.style "display" "flex"
        , Html.Attributes.style "justify-content" "center"
        , Html.Attributes.style "align-items" "center"
        , Html.Events.on "mousedown"
            (Json.Decode.map (\pos -> MousePressed (rayTo pos)) decodeMousePosition)
        , Html.Events.on "mousemove"
            (Json.Decode.map (\pos -> MouseMoved (rayTo pos)) decodeMousePosition)
        , Html.Events.onMouseUp MouseReleased
        , Html.Attributes.style "cursor"
            (case state.maybeRaycastResult of
                Just _ ->
                    "grabbing"

                Nothing ->
                    "grab"
            )
        ]
        [ Scene3d.sunny
            { upDirection = Direction3d.z
            , sunlightDirection = Direction3d.xyZ (Angle.degrees 135) (Angle.degrees -60)
            , shadows = True
            , camera = state.camera
            , dimensions =
                ( Pixels.int (round adjustedSize.width)
                , Pixels.int (round adjustedSize.height)
                )
            , background = Scene3d.backgroundColor (Color.rgb 0 0 0)
            , clipDepth = Length.meters 0.1
            , entities =
                (state.world |> Physics.World.bodies)
                    |> List.map bodyToEntity
                    |> List.map (Scene3d.scaleAbout Point3d.origin ratioScreenHeightToSceneMeter)
            }
        ]


tetrahedronWithRadius1Shadow =
    Scene3d.Mesh.shadow tetrahedronWithRadius1


bodyToEntity : Physics.Body.Body BodyKind -> Entity WorldCoordinates
bodyToEntity body =
    let
        frame =
            Physics.Body.frame body

        id =
            Physics.Body.data body
    in
    Scene3d.placeIn frame
        (case id of
            Mouse ->
                Scene3d.sphere (Material.color (Color.rgb 1 0.2 0))
                    (Sphere3d.atOrigin (millimeters 20))

            MouseImitation fixed ->
                Scene3d.sphere (Material.color (Color.rgb 1 0.2 0))
                    (Sphere3d.atOrigin (millimeters 27))

            Player ->
                Scene3d.meshWithShadow
                    (Material.color (Color.rgb 1 0.5 0.1))
                    tetrahedronWithRadius1
                    tetrahedronWithRadius1Shadow
                    |> Scene3d.scaleAbout Point3d.origin 0.125
                    |> Scene3d.rotateAround Axis3d.z (Angle.turns 0.418)

            PlayerPast ->
                Scene3d.meshWithShadow
                    (Material.color (Color.rgb 1 0.5 0.1))
                    tetrahedronWithRadius1
                    tetrahedronWithRadius1Shadow
                    |> Scene3d.scaleAbout Point3d.origin 0.125
                    |> Scene3d.rotateAround Axis3d.z (Angle.turns 0.418)

            PlayerImitation _ ->
                Scene3d.meshWithShadow
                    (Material.color (Color.rgb 1 0.5 0.1))
                    tetrahedronWithRadius1
                    tetrahedronWithRadius1Shadow
                    |> Scene3d.scaleAbout Point3d.origin 0.125

            BlockingImmovableWall dimensions ->
                Scene3d.blockWithShadow
                    (Material.color Color.white)
                    (thickLinearBlock dimensions)

            DraggableBlock dimensions ->
                Scene3d.blockWithShadow
                    (Material.color (Color.rgb 0.95 1 1))
                    (thickLinearBlock dimensions)

            DraggableBallWithCubeBehavior data ->
                Scene3d.sphereWithShadow (Material.color (Color.rgb 1 1 1))
                    (Sphere3d.atOrigin data.radius)

            Floor ->
                Scene3d.quad (Material.matte (Color.rgb 0 0 0))
                    (Point3d.meters -15 -15 0)
                    (Point3d.meters -15 15 0)
                    (Point3d.meters 15 15 0)
                    (Point3d.meters 15 -15 0)
        )


thickLinearBlock dimensions =
    let
        l =
            dimensions.length |> Length.inMeters

        w =
            0.1

        h =
            0.3
    in
    Block3d.from
        (Point3d.meters -(l / 2) 0 0)
        (Point3d.meters (l / 2) -w h)


decodeMousePosition : Decoder (Point2d Pixels WorldCoordinates)
decodeMousePosition =
    Json.Decode.map2 Point2d.pixels
        (Json.Decode.field "offsetX" Json.Decode.float)
        (Json.Decode.field "offsetY" Json.Decode.float)


blossomColorRandom : Random.Generator Color
blossomColorRandom =
    Random.map3 Color.rgb
        (Random.float 0.3 1)
        (Random.float 0.3 0.6)
        (Random.float 0.3 1)
        |> Random.Extra.filter
            (\color ->
                let
                    c =
                        color |> Color.toRgba

                    average =
                        (c.red + c.blue + c.green) / 3

                    isDifferentEnoughFromAverage component =
                        abs (component - average) > 0.2
                in
                List.any isDifferentEnoughFromAverage [ c.red, c.blue, c.green ]
            )


uiDocument : State -> Browser.Document Event
uiDocument =
    \state ->
        { title = "dragahedron"
        , body =
            state |> ui |> List.singleton
        }


withAlpha : Float -> Color -> Color
withAlpha newAlpha =
    \color ->
        let
            oldRgba =
                color |> Color.toRgba
        in
        Color.fromRgba { oldRgba | alpha = newAlpha }


backgroundColor : Color
backgroundColor =
    Color.rgb 0.03 0.05 0.09


audio : AudioData -> State -> Audio.Audio
audio audioData =
    \state ->
        -- loop
        audioWith state.audio.music
            (\music ->
                music
                    |> audioLoop
                        { initialTime = state.initialTime
                        , lastTick = state.lastTick
                        , audioData = audioData
                        }
            )
            :: (audioKinds
                    |> List.map
                        (\audioKind ->
                            audioWith (state.audio |> accessAudioOfKind audioKind)
                                (\loadedAudio ->
                                    state.audioTimes
                                        |> accessAudioOfKind audioKind
                                        |> List.map
                                            (\time -> Audio.audio loadedAudio (Duration.addTo time (Duration.seconds 0.07)))
                                        |> Audio.group
                                )
                        )
               )
            |> Audio.group


audioLoop { audioData, initialTime, lastTick } =
    \audio_ ->
        let
            audioLength =
                audio_ |> Audio.length audioData

            startTime =
                Duration.addTo
                    initialTime
                    (audioLength |> Quantity.multiplyBy (alreadyCompletedLoops |> toFloat))

            alreadyCompletedLoops =
                (Duration.from initialTime lastTick
                    |> Duration.inMilliseconds
                    |> floor
                )
                    // (audioLength |> Duration.inMilliseconds |> floor)
        in
        Audio.audio audio_ startTime


audioWith : Result error value -> (value -> Audio.Audio) -> Audio.Audio
audioWith source with =
    case source of
        Err _ ->
            Audio.silence

        Ok loadedAudio ->
            with loadedAudio



-- helpers


uiToColor : Ui.Color -> Color
uiToColor =
    \uiColor ->
        Color.fromRgba (uiColor |> Ui.toRgb)


colorToUi : Color -> Ui.Color
colorToUi =
    \color ->
        Ui.fromRgb (color |> Color.toRgba)


onJust : Maybe a -> Maybe a -> Maybe a
onJust ifNothing =
    \maybe ->
        case maybe of
            Nothing ->
                ifNothing

            Just exists ->
                Just exists


consJust : Maybe a -> List a -> List a
consJust maybeHead list =
    case maybeHead of
        Nothing ->
            list

        Just head ->
            head :: list


angleLerp : Angle -> Angle -> Float -> Angle
angleLerp a b fraction =
    degreesLerp (a |> Angle.inDegrees) (b |> Angle.inDegrees) fraction
        |> Angle.degrees


degreesLerp : Float -> Float -> Float -> Float
degreesLerp a b fraction =
    if abs (b - a) > 180 then
        if b > a then
            degreesLerp (a + 360) b fraction

        else
            degreesLerp a (b + 360) fraction

    else
        (a + ((b - a) * fraction))
            |> floatModBy 360


axisToEndPointsInWidth width axis =
    { start =
        axis
            |> Axis2d.intersectionPoint
                (Axis2d.through
                    (Point2d.fromRecord Pixels.float { x = -width / 2, y = 0 })
                    Direction2d.positiveY
                )
            |> Maybe.withDefault Point2d.origin
    , end =
        axis
            |> Axis2d.intersectionPoint
                (Axis2d.through
                    (Point2d.fromRecord Pixels.float { x = width / 2, y = 0 })
                    Direction2d.positiveY
                )
            |> Maybe.withDefault Point2d.origin
    }


floatModBy : Int -> Float -> Float
floatModBy divisor =
    \float ->
        let
            floatTruncated =
                truncate float

            floatUntruncated =
                float - (floatTruncated |> toFloat)
        in
        (floatTruncated |> modBy divisor |> toFloat) + floatUntruncated


isLeftOfLineSegment :
    LineSegment2d Pixels Float
    -> Point2d Pixels Float
    -> Bool
isLeftOfLineSegment lineSegment point =
    let
        a =
            lineSegment |> LineSegment2d.startPoint |> Point2d.toRecord Pixels.toFloat

        b =
            lineSegment |> LineSegment2d.endPoint |> Point2d.toRecord Pixels.toFloat

        c =
            point |> Point2d.toRecord Pixels.toFloat
    in
    ((b.x - a.x) * (c.y - a.y) - (b.y - a.y) * (c.x - a.x)) > 0


onLineSegment2dClosestTo :
    Point2d Pixels Float
    -> LineSegment2d Pixels Float
    ->
        { distance : Quantity Float Pixels
        , pointOnLine : Point2d Pixels Float
        }
onLineSegment2dClosestTo point lineSegment =
    if point |> isLeftOfLineSegment lineSegment then
        onLineSegment2dClosestTo point (LineSegment2d.from (lineSegment |> LineSegment2d.endPoint) (lineSegment |> LineSegment2d.startPoint))

    else
        let
            distance =
                let
                    startToPoint : Vector2d Pixels Float
                    startToPoint =
                        Vector2d.from (lineSegment |> LineSegment2d.startPoint) point

                    lineSegmentVector : Vector2d Pixels Float
                    lineSegmentVector =
                        Vector2d.from (lineSegment |> LineSegment2d.startPoint) (lineSegment |> LineSegment2d.endPoint)

                    fractionAlongLineSegment =
                        (startToPoint |> Vector2d.xComponent)
                            |> Quantity.times (lineSegmentVector |> Vector2d.xComponent)
                            |> Quantity.plus
                                ((startToPoint |> Vector2d.yComponent)
                                    |> Quantity.times (lineSegmentVector |> Vector2d.yComponent)
                                )
                            |> Quantity.over (lineSegmentVector |> vector2dLengthSquared)
                            |> Quantity.clamp (Pixels.float 0) (Pixels.float 1)

                    lineSegmentFraction : Vector2d Pixels Float
                    lineSegmentFraction =
                        lineSegmentVector |> Vector2d.scaleBy (fractionAlongLineSegment |> Pixels.toFloat)

                    fractionEndPoint : Point2d Pixels Float
                    fractionEndPoint =
                        lineSegment |> LineSegment2d.startPoint |> Point2d.translateBy lineSegmentFraction

                    distanceVector : Vector2d Pixels Float
                    distanceVector =
                        Vector2d.from fractionEndPoint point
                in
                distanceVector |> Vector2d.length
        in
        { distance = distance
        , pointOnLine =
            point
                |> Point2d.translateBy
                    (Vector2d.withLength distance
                        (lineSegment
                            |> LineSegment2d.perpendicularDirection
                            |> Maybe.withDefault (Direction2d.fromAngle (Angle.turns 0.25))
                        )
                    )
        }


vector2dLengthSquared : Vector2d Pixels Float -> Quantity Float Pixels
vector2dLengthSquared =
    \vector2d ->
        vector2d
            |> Vector2d.xComponent
            |> Quantity.squared
            |> Quantity.plus (vector2d |> Vector2d.yComponent |> Quantity.squared)
            |> Quantity.over Pixels.pixel


pointsToSegments : List (Point2d units coordinates) -> List (LineSegment2d units coordinates)
pointsToSegments =
    \points ->
        case points of
            [] ->
                []

            startPoint :: afterStartPoint ->
                afterStartPoint
                    |> List.foldl
                        (\point soFar ->
                            { segments =
                                soFar.segments
                                    |> (::) (LineSegment2d.from soFar.newStart point)
                            , newStart = point
                            }
                        )
                        { segments = [], newStart = startPoint }
                    |> .segments


untilLengthFrom lengthUpperLimit =
    \points ->
        case points of
            [] ->
                []

            originPoint :: afterOriginPoint ->
                afterOriginPoint
                    |> List.Extra.stoppableFoldl
                        (\newEnd soFar ->
                            let
                                newLengthSquared =
                                    soFar.lengthSquared
                                        + (Vector2d.from soFar.end newEnd
                                            |> vector2dLengthSquared
                                            |> Pixels.toFloat
                                          )
                            in
                            if newLengthSquared < (lengthUpperLimit * lengthUpperLimit) then
                                { lengthSquared = newLengthSquared
                                , points = soFar.points |> (::) newEnd
                                , end = newEnd
                                }
                                    |> List.Extra.Continue

                            else
                                { soFar | points = soFar.points |> (::) newEnd } |> List.Extra.Stop
                        )
                        { lengthSquared = 0
                        , points = []
                        , end = originPoint
                        }
                    |> .points
                    |> List.reverse
                    |> (::) originPoint


dictUnionBy : (a -> a -> a) -> Dict comparableKey a -> Dict comparableKey a -> Dict comparableKey a
dictUnionBy combineAB aDict bDict =
    Dict.merge
        (\k a soFar -> soFar |> Dict.insert k a)
        (\k a b soFar -> soFar |> Dict.insert k (combineAB a b))
        (\k b soFar -> soFar |> Dict.insert k b)
        aDict
        bDict
        Dict.empty
