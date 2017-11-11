module RoverTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)


type alias Point =
    { x : Int, y : Int }


type Direction
    = North
    | East
    | South
    | West


type alias Scenario =
    { rover : Rover, commands : List Command }


type Command
    = Forward
    | Backward


type alias Rover =
    ( Point, Direction )


receiveCommands : Scenario -> Rover
receiveCommands { rover, commands } =
    case commands of
        [] ->
            rover

        command :: _ ->
            let
                originalLocation =
                    location rover

                originalOrientation =
                    orientation rover

                newPoint =
                    move <| Movement originalLocation command
            in
                ( newPoint, originalOrientation )


type alias Movement =
    { startingLocation : Point, command : Command }


move : Movement -> Point
move { startingLocation, command } =
    let
        currentLocation =
            startingLocation
    in
        case command of
            Forward ->
                moveNorth currentLocation

            _ ->
                moveSouth currentLocation


moveNorth : Point -> Point
moveNorth point =
    { point | y = point.y + 1 }


moveSouth : Point -> Point
moveSouth point =
    { point | y = -1 }


location : Rover -> Point
location ( point, direction ) =
    point


orientation : Rover -> Direction
orientation ( point, direction ) =
    direction


suite : Test
suite =
    let
        startingPoint =
            Point 0 0

        startingDirection =
            North

        initialRover =
            ( startingPoint, startingDirection )
    in
        describe "Rover"
            [ describe
                "given no commands"
                [ test "should stay put" <|
                    \() ->
                        let
                            noActionScenario =
                                Scenario initialRover []
                        in
                            Expect.equal initialRover <| receiveCommands noActionScenario
                ]
            , describe "facing North, given one forward command"
                [ test "should move North one space" <|
                    \() ->
                        let
                            moveForwardScenario =
                                Scenario initialRover [ Forward ]
                        in
                            Expect.equal ( (Point 0 1), North ) <| receiveCommands moveForwardScenario
                ]
            , describe "facing North, given one backward command"
                [ test "should move South one space" <|
                    \() ->
                        let
                            moveForwardScenario =
                                Scenario initialRover [ Backward ]
                        in
                            Expect.equal ( (Point 0 -1), North ) <| receiveCommands moveForwardScenario
                ]
            , describe "facing South, given one forward command"
                [ test "should move South one space" <|
                    \() ->
                        let
                            moveForwardScenario =
                                Scenario ({ initialRover | direction = South }) [ Backward ]
                        in
                            Expect.equal ( (Point 0 -1), North ) <| receiveCommands moveForwardScenario
                ]

            -- , describe "with direction West, given one forward command"
            --     [ test "should move West one space" <|
            --         \() ->
            --             let
            --                 moveForwardScenario =
            --                     Scenario initialRover [ Forward ]
            --             in
            --                 Expect.equal initialRover <| receiveCommands moveForwardScenario
            --     ]
            ]
