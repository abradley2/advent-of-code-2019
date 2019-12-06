module Day3 exposing (main)

import Browser exposing (element)
import Html as H
import Html.Attributes as A
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
                            ( Set.insert latest coordSet, prevOrderedCoords, latest )
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
                            ( Set.insert latest coordSet, prevOrderedCoords, latest )
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
                            ( Set.insert latest coordSet, prevOrderedCoords, latest )
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
                            ( Set.insert latest coordSet, prevOrderedCoords, latest )
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
