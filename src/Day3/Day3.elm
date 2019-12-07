module Day3 exposing (main)

import Array exposing (Array)
import Basics.Extra exposing (flip)
import Browser exposing (element)
import Dict exposing (Dict)
import Html as H
import Html.Attributes as A
import List.Extra as ListX
import Parser exposing ((|.), (|=), Parser)
import Platform exposing (Program)
import Result.Extra as ResultX
import Set exposing (Set)


type alias Problem =
    { input : String
    , part : Int
    }


type alias Solution =
    Result String String


type Direction
    = Up
    | Down
    | Left
    | Right


type alias Instruction =
    { direction : Direction
    , length : Int
    }


type alias Coords =
    ( Int, Int )


centralPort : Coords
centralPort =
    ( 0, 0 )


parseInstruction : Parser Instruction
parseInstruction =
    Parser.succeed
        Instruction
        |= Parser.oneOf
            [ Parser.symbol "U" |> Parser.map (\_ -> Up)
            , Parser.symbol "D" |> Parser.map (\_ -> Down)
            , Parser.symbol "L" |> Parser.map (\_ -> Left)
            , Parser.symbol "R" |> Parser.map (\_ -> Right)
            ]
        |= Parser.int
        |. Parser.end


lineToInstructions : String -> Result (List Parser.DeadEnd) (List Instruction)
lineToInstructions input =
    input
        |> String.split ","
        |> List.map (Parser.run parseInstruction)
        |> ResultX.combine


{-| Given a start, end, and functor- outputs a new list until functor returns ends
-}
getRange : Int -> Int -> List Int
getRange start end =
    if start > end then
        List.range end start |> List.reverse

    else
        List.range start end


instructionToCoords : Instruction -> InstructionGenerator -> InstructionGenerator
instructionToCoords instruction acc =
    let
        ( curX, curY ) =
            acc.currentCoords
    in
    case instruction.direction of
        Up ->
            let
                range =
                    getRange curY (curY - instruction.length)

                ( coords, orderedCoords, currentCoords ) =
                    List.foldl
                        (\nextY ( coordSet, prevOrderedCoords, _ ) ->
                            let
                                latest =
                                    ( curX, nextY )
                            in
                            ( Set.insert latest coordSet, prevOrderedCoords ++ [ latest ], latest )
                        )
                        ( Set.empty, [], acc.currentCoords )
                        range
            in
            { coords = Set.union coords acc.coords
            , currentCoords = currentCoords
            , orderedCoords = orderedCoords
            }

        Down ->
            let
                range =
                    getRange curY (curY + instruction.length)

                ( coords, orderedCoords, currentCoords ) =
                    List.foldl
                        (\nextY ( coordSet, prevOrderedCoords, _ ) ->
                            let
                                latest =
                                    ( curX, nextY )
                            in
                            ( Set.insert latest coordSet, prevOrderedCoords ++ [ latest ], latest )
                        )
                        ( Set.empty, [], acc.currentCoords )
                        range
            in
            { coords = Set.union coords acc.coords
            , currentCoords = currentCoords
            , orderedCoords = orderedCoords
            }

        Left ->
            let
                range =
                    getRange curX (curX - instruction.length)

                ( coords, orderedCoords, currentCoords ) =
                    List.foldl
                        (\nextX ( coordSet, prevOrderedCoords, _ ) ->
                            let
                                latest =
                                    ( nextX, curY )
                            in
                            ( Set.insert latest coordSet, prevOrderedCoords ++ [ latest ], latest )
                        )
                        ( Set.empty, [], acc.currentCoords )
                        range
            in
            { coords = Set.union coords acc.coords
            , currentCoords = currentCoords
            , orderedCoords = orderedCoords
            }

        Right ->
            let
                range =
                    getRange curX (curX + instruction.length)

                ( coords, orderedCoords, currentCoords ) =
                    List.foldl
                        (\nextX ( coordSet, prevOrderedCoords, _ ) ->
                            let
                                latest =
                                    ( nextX, curY )
                            in
                            ( Set.insert latest coordSet, prevOrderedCoords ++ [ latest ], latest )
                        )
                        ( Set.empty, [], acc.currentCoords )
                        range
            in
            { coords = Set.union coords acc.coords
            , currentCoords = currentCoords
            , orderedCoords = orderedCoords
            }


type alias InstructionGenerator =
    { currentCoords : Coords
    , coords : Set Coords
    , orderedCoords : List Coords
    }


instructionsToCoords : List Instruction -> Set Coords
instructionsToCoords instructions =
    List.foldl
        instructionToCoords
        { coords = Set.empty, currentCoords = centralPort, orderedCoords = [] }
        instructions
        |> .coords


coordsToDist : Coords -> Int
coordsToDist ( x, y ) =
    abs x + abs y


walkInstructions : Coords -> Array Instruction -> Int -> Int -> Coords -> Result String Int
walkInstructions targetIntersection instructions currentIndex currentDistance currentCoords =
    let
        instruction =
            Array.get currentIndex instructions

        incomingCoords =
            Maybe.map
                (\val ->
                    instructionToCoords
                        val
                        { coords = Set.empty, currentCoords = currentCoords, orderedCoords = [] }
                        |> .orderedCoords
                )
                instruction
                |> Maybe.withDefault []

        incomingMatch =
            ListX.find ((==) targetIntersection) incomingCoords
                |> Maybe.andThen
                    (\val ->
                        if val == centralPort then
                            Nothing

                        else
                            Just val
                    )

        nextCoordsAndDistance =
            Maybe.map2
                Tuple.pair
                (ListX.last incomingCoords)
                (Maybe.map (.length >> (+) currentDistance) instruction)
    in
    case ( instruction, incomingMatch, nextCoordsAndDistance ) of
        ( _, Just firstMatch, _ ) ->
            let
                dist =
                    ListX.elemIndex firstMatch incomingCoords |> Maybe.withDefault 0

                totalDist =
                    currentDistance + dist
            in
            Result.Ok totalDist

        ( _, Nothing, Just ( nextCoords, nextDistance ) ) ->
            walkInstructions
                targetIntersection
                instructions
                (currentIndex + 1)
                nextDistance
                nextCoords

        ( _, _, _ ) ->
            Result.Err "out of index and couldnt find match when walking instructions"


walkIntersections : ( Array Instruction, Array Instruction ) -> Coords -> Dict Coords Int -> Dict Coords Int
walkIntersections ( wireA, wireB ) currentIntersection results =
    let
        distanceResults =
            Result.map2
                Tuple.pair
                (walkInstructions currentIntersection wireA 0 0 centralPort)
                (walkInstructions currentIntersection wireB 0 0 centralPort)
    in
    Result.map
        (\( distA, distB ) -> Dict.insert currentIntersection (distA + distB) results)
        distanceResults
        |> Result.withDefault results


partTwo : String -> Solution
partTwo input =
    let
        knownIntersections =
            Result.map2
                Tuple.pair
                (String.split "\n" input
                    |> List.head
                    |> Result.fromMaybe []
                    |> Result.andThen lineToInstructions
                    |> Result.map instructionsToCoords
                )
                (String.split "\n" input
                    |> List.reverse
                    |> List.head
                    |> Result.fromMaybe []
                    |> Result.andThen lineToInstructions
                    |> Result.map instructionsToCoords
                )
                |> Result.map
                    (\( wireA, wireB ) ->
                        Set.intersect wireA wireB
                    )

        instructionListA =
            String.split "\n" input
                |> List.head
                |> Result.fromMaybe []
                |> Result.andThen lineToInstructions

        instructionListB =
            String.split "\n" input
                |> ListX.last
                |> Result.fromMaybe []
                |> Result.andThen lineToInstructions
    in
    Result.map2
        Tuple.pair
        knownIntersections
        (Result.map2 Tuple.pair instructionListA instructionListB)
        |> Result.mapError (\_ -> "Failed to parse")
        |> Result.map
            (\( intersections_, ( instructionsA_, instructionsB_ ) ) ->
                List.foldr
                    (walkIntersections ( Array.fromList instructionsA_, Array.fromList instructionsB_ ))
                    Dict.empty
                    (Set.toList intersections_)
            )
        |> Result.map
            (\resultDict ->
                List.foldr
                    (\current champion ->
                        if current < champion || champion == 0 then
                            current

                        else
                            champion
                    )
                    0
                    (Dict.values resultDict)
                    |> String.fromInt
            )


partOne : String -> Solution
partOne input =
    let
        wires =
            Result.map2
                Tuple.pair
                (String.split "\n" input
                    |> List.head
                    |> Result.fromMaybe []
                    |> Result.andThen lineToInstructions
                    |> Result.map instructionsToCoords
                )
                (String.split "\n" input
                    |> List.reverse
                    |> List.head
                    |> Result.fromMaybe []
                    |> Result.andThen lineToInstructions
                    |> Result.map instructionsToCoords
                )
                |> Result.map
                    (\( wireA, wireB ) ->
                        Set.intersect wireA wireB
                    )
                |> Result.map Set.toList
                |> Result.map
                    (List.foldl
                        (\sut champion ->
                            if
                                (coordsToDist sut < coordsToDist champion)
                                    || (coordsToDist champion == 0)
                            then
                                sut

                            else
                                champion
                        )
                        ( 0, 0 )
                    )
                |> Result.map (coordsToDist >> String.fromInt)
    in
    wires
        |> Result.mapError (\_ -> "Failed to parse")


solve : Problem -> Solution
solve problem =
    case problem.part of
        1 ->
            partOne problem.input

        2 ->
            partTwo problem.input

        _ ->
            Result.Err
                ("No solution for problem number = "
                    ++ String.fromInt problem.part
                )


main : Program Problem Solution Never
main =
    element
        { init = solve >> (\model -> ( model, Cmd.none ))
        , view = Result.map H.text >> Result.mapError H.text >> ResultX.merge
        , update = update
        , subscriptions = \_ -> Sub.none
        }


update : Never -> Solution -> ( Solution, Cmd Never )
update _ model =
    ( model, Cmd.none )


type Msg
    = NoOp
