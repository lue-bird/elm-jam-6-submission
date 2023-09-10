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
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Json.Decode exposing (Decoder)
import Json.Encode
import Key
import Length exposing (Meters, millimeters)
import LineSegment2d exposing (LineSegment2d)
import List.Extra
import Mass exposing (kilograms)
import Physics.Body as Body exposing (Body)
import Physics.Constraint as Constraint
import Physics.Coordinates exposing (BodyCoordinates, WorldCoordinates)
import Physics.Shape
import Physics.World as World exposing (World)
import Pixels exposing (Pixels, pixels)
import Plane3d
import Point2d exposing (Point2d)
import Point3d
import Quantity exposing (Quantity, Rate)
import Random
import Random.Extra
import Reaction exposing (Reaction)
import RecordWithoutConstructorFunction exposing (RecordWithoutConstructorFunction)
import Rectangle2d
import Scene3d exposing (Entity)
import Scene3d.Material as Material
import Sphere3d
import Task
import Time
import Tree exposing (Tree)
import Tree.Path exposing (TreePath)
import Vector2d exposing (Vector2d)
import Viewpoint3d
import VirtualDom


type SpecificOrShared specific shared
    = Specific specific
    | Shared shared


type alias SpecificAndShared specific shared =
    RecordWithoutConstructorFunction
        { shared : shared
        , specific : specific
        }


type MenuOrGame menu game
    = Menu menu
    | Game game


type alias Event =
    SpecificOrShared EventSpecific EventShared


type EventShared
    = AudioLoaded { piece : AudioKind, result : Result Audio.LoadError Audio.Source }


type alias EventSpecific =
    MenuOrGame MenuEvent GameEvent


type MenuEvent
    = GameStartClicked


type GameEvent
    = GameWindowSized { width : Float, height : Float }
    | InitialRandomSeedReceived Random.Seed
    | InitialTimeReceived Time.Posix
    | FrameTickPassed Time.Posix
    | KeyPressed Key.Key
    | KeyReleased Key.Key
    | MousePressed (Axis3d Meters WorldCoordinates)
    | MouseMoved (Axis3d Meters WorldCoordinates)
    | MouseReleased


type alias State =
    SpecificAndShared StateSpecific StateShared


type alias StateShared =
    RecordWithoutConstructorFunction
        { audio : EachAudio (Result Audio.LoadError Audio.Source)
        }


type alias StateSpecific =
    MenuOrGame MenuState GameState


type alias MenuState =
    RecordWithoutConstructorFunction
        {}


type alias GameState =
    RecordWithoutConstructorFunction
        { windowSize : { width : Float, height : Float }
        , audioTimes : EachAudio (List Time.Posix)
        , keysPressed : List Key.Key
        , randomSeed : Random.Seed
        , lastTick : Time.Posix
        , initialTime : Time.Posix
        , world : World BodyKind
        , maybeRaycastResult : Maybe (World.RaycastResult BodyKind)
        }


type alias Effect =
    SpecificOrShared EffectSpecific EffectShared


type alias EffectSpecific =
    MenuOrGame MenuEffect GameEffect


type EffectShared
    = LoadAudio AudioKind


type MenuEffect
    = MenuEffectNoneYet Never


type GameEffect
    = RequestInitialRandomSeed
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
    = AudioMirrorGrow
    | AudioMirrorPlace
    | AudioMirrorGrab
    | AudioMusic


audioKinds : List AudioKind
audioKinds =
    [ AudioMirrorGrow, AudioMirrorPlace, AudioMirrorGrab, AudioMusic ]


type alias EachAudio perKind =
    { mirrorGrow : perKind
    , mirrorPlace : perKind
    , mirrorGrab : perKind
    , music : perKind
    }


eachAudio : perKind -> EachAudio perKind
eachAudio perKind =
    { mirrorGrow = perKind
    , mirrorPlace = perKind
    , mirrorGrab = perKind
    , music = perKind
    }


alterAudioOfKind : AudioKind -> (a -> a) -> EachAudio a -> EachAudio a
alterAudioOfKind kind f =
    case kind of
        AudioMirrorGrow ->
            \r -> { r | mirrorGrow = r.mirrorGrow |> f }

        AudioMirrorPlace ->
            \r -> { r | mirrorPlace = r.mirrorPlace |> f }

        AudioMirrorGrab ->
            \r -> { r | mirrorGrab = r.mirrorGrab |> f }

        AudioMusic ->
            \r -> { r | music = r.music |> f }


accessAudioOfKind : AudioKind -> EachAudio a -> a
accessAudioOfKind kind =
    case kind of
        AudioMirrorGrow ->
            .mirrorGrow

        AudioMirrorPlace ->
            .mirrorPlace

        AudioMirrorGrab ->
            .mirrorGrab

        AudioMusic ->
            .music


audioPieceToName : AudioKind -> String
audioPieceToName =
    \audioPiece ->
        case audioPiece of
            AudioMirrorGrow ->
                "mirror-grow"

            AudioMirrorPlace ->
                "mirror-place"

            AudioMirrorGrab ->
                "mirror-grab"

            AudioMusic ->
                "music"


init : () -> Reaction State Effect
init () =
    Reaction.to
        { specific = Menu {}
        , shared =
            { audio = eachAudio (Err Audio.UnknownError) }
        }
        |> Reaction.effectsAdd
            (audioKinds |> List.map (\piece -> LoadAudio piece |> Shared))


initGame : Reaction GameState GameEffect
initGame =
    Reaction.to
        { windowSize =
            -- dummy
            { width = 0, height = 0 }
        , audioTimes = eachAudio []
        , world = initialWorld
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
        }
        |> Reaction.effectsAdd
            [ RequestInitialRandomSeed
            , RequestInitialTime
            , GameRequestInitialWindowSize
            ]


withShared : shared -> specific -> { specific : specific, shared : shared }
withShared shared =
    \specific -> { specific = specific, shared = shared }


reactTo : Event -> (State -> Reaction State Effect)
reactTo event =
    case event of
        Shared eventShared ->
            \state ->
                reactToShared eventShared state
                    |> Reaction.effectMap Shared

        Specific eventSpecific ->
            \state ->
                reactToSpecific eventSpecific state


reactToSpecific : EventSpecific -> (State -> Reaction State Effect)
reactToSpecific eventSpecific =
    case eventSpecific of
        Menu menuEvent ->
            \state ->
                case state.specific of
                    Menu menuState ->
                        menuState
                            |> menuReactTo menuEvent
                            |> Reaction.map (withShared state.shared)

                    _ ->
                        Reaction.to state

        Game gameEvent ->
            \state ->
                case state.specific of
                    Game gameState ->
                        gameState
                            |> gameReactTo gameEvent
                            |> Reaction.map (withShared state.shared)

                    _ ->
                        Reaction.to state


reactToShared : EventShared -> (State -> Reaction State EffectShared)
reactToShared eventShared =
    case eventShared of
        AudioLoaded audioLoaded ->
            \state ->
                Reaction.to
                    { state
                        | shared =
                            { audio =
                                state.shared.audio
                                    |> alterAudioOfKind audioLoaded.piece (\_ -> audioLoaded.result)
                            }
                    }


menuReactTo : MenuEvent -> (MenuState -> Reaction StateSpecific Effect)
menuReactTo menuEvent =
    case menuEvent of
        GameStartClicked ->
            \_ -> initGame |> Reaction.map Game |> Reaction.effectMap (Game >> Specific)


gameReactTo : GameEvent -> (GameState -> Reaction StateSpecific Effect)
gameReactTo event =
    case event of
        GameWindowSized size ->
            \state -> Reaction.to ({ state | windowSize = size } |> Game)

        InitialRandomSeedReceived initialRandomSeed ->
            \state ->
                Reaction.to
                    ({ state | randomSeed = initialRandomSeed }
                        |> Game
                    )

        InitialTimeReceived initialTime ->
            \state ->
                Reaction.to
                    ({ state
                        | initialTime = initialTime
                        , lastTick = initialTime
                     }
                        |> Game
                    )

        FrameTickPassed newSimulationTime ->
            \state ->
                let
                    sinceLastTick =
                        Duration.from state.lastTick newSimulationTime
                in
                Reaction.to
                    ({ state
                        | lastTick = newSimulationTime
                        , world = World.simulate (seconds (1 / 60)) state.world
                     }
                        |> Game
                    )

        KeyPressed key ->
            \state ->
                Reaction.to
                    ({ state | keysPressed = state.keysPressed |> (::) key }
                        |> Game
                    )

        KeyReleased key ->
            \state ->
                Reaction.to
                    ({ state
                        | keysPressed =
                            state.keysPressed |> List.filter (\keyPressed -> keyPressed /= key)
                     }
                        |> Game
                    )

        MousePressed mouseRay ->
            \model ->
                case World.raycast mouseRay model.world of
                    Just raycastResult ->
                        if raycastResult.body |> Body.data |> isDraggable then
                            let
                                worldPoint =
                                    Point3d.placeIn
                                        (Body.frame raycastResult.body)
                                        raycastResult.point

                                mouse =
                                    Body.compound [] Mouse
                                        |> Body.moveTo worldPoint
                            in
                            Reaction.to
                                ({ model
                                    | maybeRaycastResult = Just raycastResult
                                    , world =
                                        model.world
                                            |> World.add mouse
                                            |> World.constrain
                                                (\b1 b2 ->
                                                    case Body.data b1 of
                                                        Mouse ->
                                                            if (b2 |> Body.data) == (raycastResult.body |> Body.data) then
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
                                    |> Game
                                )

                        else
                            Reaction.to (model |> Game)

                    Nothing ->
                        Reaction.to (model |> Game)

        MouseMoved mouseRay ->
            \state ->
                case state.maybeRaycastResult of
                    Just raycastResult ->
                        let
                            worldPoint =
                                Point3d.placeIn
                                    (Body.frame raycastResult.body)
                                    raycastResult.point

                            plane =
                                Plane3d.through
                                    worldPoint
                                    (Viewpoint3d.viewDirection (Camera3d.viewpoint camera))
                        in
                        Reaction.to
                            ({ state
                                | world =
                                    World.update
                                        (\body ->
                                            if Body.data body == Mouse then
                                                case Axis3d.intersectionWithPlane plane mouseRay of
                                                    Just intersection ->
                                                        Body.moveTo intersection body

                                                    Nothing ->
                                                        body

                                            else
                                                body
                                        )
                                        state.world
                             }
                                |> Game
                            )

                    Nothing ->
                        Reaction.to (state |> Game)

        MouseReleased ->
            \state ->
                Reaction.to
                    ({ state
                        | maybeRaycastResult = Nothing
                        , world =
                            World.keepIf
                                (\body -> Body.data body /= Mouse)
                                state.world
                     }
                        |> Game
                    )


subscriptions : State -> Sub Event
subscriptions =
    \state ->
        case state.specific of
            Menu _ ->
                Sub.none

            Game _ ->
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
                    |> Sub.map (Game >> Specific)


port audioPortToJS : Json.Encode.Value -> Cmd msg_


port audioPortFromJS : (Json.Decode.Value -> msg) -> Sub msg


interpretEffect : Effect -> Reaction.EffectInterpretation Event
interpretEffect =
    \effect ->
        case effect of
            Specific effectSpecific ->
                effectSpecific |> interpretEffectSpecific

            Shared effectShared ->
                effectShared |> interpretEffectShared |> Reaction.effectInterpretationMap Shared


interpretEffectSpecific : EffectSpecific -> Reaction.EffectInterpretation Event
interpretEffectSpecific =
    \effectSpecific ->
        case effectSpecific of
            Menu menuEffect ->
                menuEffect |> menuInterpretEffect |> Reaction.effectInterpretationMap (Menu >> Specific)

            Game gameEffect ->
                gameEffect |> gameInterpretEffect |> Reaction.effectInterpretationMap (Game >> Specific)


interpretEffectShared : EffectShared -> Reaction.EffectInterpretation EventShared
interpretEffectShared =
    \effectShared ->
        case effectShared of
            LoadAudio piece ->
                Reaction.audioCommands
                    [ Audio.loadAudio
                        (\result -> AudioLoaded { result = result, piece = piece })
                        ([ "public/", piece |> audioPieceToName, ".mp3" ] |> String.concat)
                    ]


menuInterpretEffect : MenuEffect -> Reaction.EffectInterpretation MenuEvent
menuInterpretEffect =
    \effect ->
        case effect of
            MenuEffectNoneYet ever ->
                never ever


gameInterpretEffect : GameEffect -> Reaction.EffectInterpretation GameEvent
gameInterpretEffect =
    \effect ->
        case effect of
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
    | Ball
    | Floor
    | Table


isDraggable : BodyKind -> Bool
isDraggable kind =
    case kind of
        Ball ->
            True

        Table ->
            True

        Mouse ->
            False

        Floor ->
            False


initialWorld : World BodyKind
initialWorld =
    World.empty
        |> World.withGravity (Acceleration.gees 1) Direction3d.negativeZ
        |> World.add table
        |> World.add (Body.plane Floor)
        |> World.add
            (Body.compound [ Physics.Shape.sphere ballShape ] Ball
                |> Body.withBehavior (Body.dynamic (kilograms 25))
                |> Body.moveTo (Point3d.fromMeters { x = 0, y = 0, z = 0.8 })
            )


tableBlocks : List (Block3d Meters BodyCoordinates)
tableBlocks =
    let
        leg =
            50

        y =
            300

        x =
            1000

        top =
            400
    in
    [ Block3d.from
        (Point3d.millimeters x y 0)
        (Point3d.millimeters (x + leg) (y + leg) top)
    , Block3d.from
        (Point3d.millimeters -(x + leg) y 0)
        (Point3d.millimeters -x (y + leg) top)
    , Block3d.from
        (Point3d.millimeters -(x + leg) -(y + leg) 0)
        (Point3d.millimeters -x -y top)
    , Block3d.from
        (Point3d.millimeters x -(y + leg) 0)
        (Point3d.millimeters (x + leg) -y top)
    , Block3d.from
        (Point3d.millimeters -(x + leg) -(y + leg) top)
        (Point3d.millimeters (x + leg) (y + leg) (top + 50))
    ]


table : Body BodyKind
table =
    Body.compound (List.map Physics.Shape.block tableBlocks) Table
        |> Body.withBehavior (Body.dynamic (kilograms 80))


camera : Camera3d Meters WorldCoordinates
camera =
    Camera3d.perspective
        { viewpoint =
            Viewpoint3d.lookAt
                { eyePoint = Point3d.meters 3 4 2
                , focalPoint = Point3d.meters -0.5 -0.5 0
                , upDirection = Direction3d.positiveZ
                }
        , verticalFieldOfView = Angle.degrees 24
        }


gameUi : GameState -> Html GameEvent
gameUi state =
    Html.div
        [ Html.Attributes.style "position" "absolute"
        , Html.Attributes.style "left" "0"
        , Html.Attributes.style "top" "0"
        , Html.Events.on "mousedown" (Json.Decode.map MousePressed (decodeMouseRay camera state.windowSize))
        , Html.Events.on "mousemove" (Json.Decode.map MouseMoved (decodeMouseRay camera state.windowSize))
        , Html.Events.onMouseUp MouseReleased
        ]
        [ Scene3d.sunny
            { upDirection = Direction3d.z
            , sunlightDirection = Direction3d.xyZ (Angle.degrees 135) (Angle.degrees -60)
            , shadows = True
            , camera = camera
            , dimensions =
                ( Pixels.int (round state.windowSize.width)
                , Pixels.int (round state.windowSize.height)
                )
            , background = Scene3d.transparentBackground
            , clipDepth = Length.meters 0.1
            , entities = List.map bodyToEntity (state.world |> World.bodies)
            }
        ]


ballShape =
    Sphere3d.atOrigin (Length.meters 0.1)


bodyToEntity : Body BodyKind -> Entity WorldCoordinates
bodyToEntity body =
    let
        frame =
            Body.frame body

        id =
            Body.data body
    in
    Scene3d.placeIn frame
        (case id of
            Mouse ->
                Scene3d.sphere (Material.color Color.black)
                    (Sphere3d.atOrigin (millimeters 30))

            Ball ->
                Scene3d.sphereWithShadow (Material.color (Color.rgb 0 0.5 1))
                    ballShape

            Table ->
                tableBlocks
                    |> List.map
                        (Scene3d.blockWithShadow
                            (Material.color Color.white)
                        )
                    |> Scene3d.group

            Floor ->
                Scene3d.quad (Material.matte Color.darkCharcoal)
                    (Point3d.meters -15 -15 0)
                    (Point3d.meters -15 15 0)
                    (Point3d.meters 15 15 0)
                    (Point3d.meters 15 -15 0)
        )


decodeMouseRay :
    Camera3d Meters WorldCoordinates
    -> { width : Float, height : Float }
    -> Decoder (Axis3d Meters WorldCoordinates)
decodeMouseRay camera3d windowSize =
    Json.Decode.map2
        (\x y ->
            Camera3d.ray
                camera3d
                (Rectangle2d.with
                    { x1 = pixels 0
                    , y1 = Pixels.float windowSize.height
                    , x2 = Pixels.float windowSize.width
                    , y2 = pixels 0
                    }
                )
                (Point2d.pixels x y)
        )
        (Json.Decode.field "pageX" Json.Decode.float)
        (Json.Decode.field "pageY" Json.Decode.float)


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
        { title = "ball reach right"
        , body =
            state.specific |> ui |> List.singleton
        }


ui : StateSpecific -> Html Event
ui =
    \state ->
        case state of
            Menu menuState ->
                menuState
                    |> menuUi
                    |> Ui.layout
                        [ UiBackground.color (backgroundColor |> colorToUi)
                        , UiFont.color (Ui.rgb 1 1 1)
                        ]
                    |> Html.map (Menu >> Specific)

            Game gameState ->
                gameState
                    |> gameUi
                    |> Html.map (Game >> Specific)


menuUi : MenuState -> Ui.Element MenuEvent
menuUi =
    \state ->
        Ui.column
            [ Ui.spacing 50
            , Ui.centerX
            , Ui.centerY
            ]
            [ Ui.text "d#ia#ry"
                |> Ui.el
                    [ UiFont.size 50
                    , UiFont.family [ UiFont.monospace ]
                    ]
            , Ui.row
                [ Ui.spacing 5
                , Ui.centerX
                , Ui.centerY
                , UiFont.size 55
                , UiFont.extraBold
                ]
                (List.range 0 6
                    |> List.map
                        (\weekday ->
                            Ui.column
                                [ Ui.spacing 5
                                , Ui.centerX
                                , Ui.centerY
                                , UiFont.size 55
                                , UiFont.extraBold
                                ]
                                (List.range 0 3
                                    |> List.map
                                        (\week ->
                                            UiInput.button
                                                [ Ui.paddingXY 50 30
                                                , UiBackground.color (Ui.rgba 1 1 1 0.02)
                                                , UiBorder.rounded 100
                                                , Ui.width Ui.fill
                                                ]
                                                { onPress = GameStartClicked |> Just
                                                , label =
                                                    Ui.text "🌼"
                                                        |> Ui.el
                                                            [ Ui.centerX
                                                            , Html.Attributes.style "transform" "scale(-1, -1)" |> Ui.htmlAttribute
                                                            , Ui.inFront
                                                                (((1 + week) * (1 + weekday))
                                                                    |> String.fromInt
                                                                    |> Ui.text
                                                                    |> Ui.el
                                                                        [ Ui.moveDown 50
                                                                        , Ui.moveRight 60
                                                                        , UiFont.size 20
                                                                        ]
                                                                )
                                                            ]
                                                }
                                        )
                                )
                        )
                )
            ]


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


audio : AudioData -> State -> Audio.Audio
audio audioData =
    \state ->
        case state.specific of
            Menu _ ->
                Audio.silence

            Game gameState ->
                -- loop
                audioWith state.shared.audio.music
                    (\music ->
                        music
                            |> audioLoop
                                { initialTime = gameState.initialTime
                                , lastTick = gameState.lastTick
                                , audioData = audioData
                                }
                    )
                    :: (audioKinds
                            |> List.map
                                (\audioKind ->
                                    audioWith (state.shared.audio |> accessAudioOfKind audioKind)
                                        (\loadedAudio ->
                                            gameState.audioTimes
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
