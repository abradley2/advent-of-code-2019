module Day3 exposing (main)

import Browser exposing (element)
import Html as H
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
    ( 500, 500 )


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


getRange : Int -> Int -> (Int -> Int) -> List Int -> List Int
getRange start end reduce acc =
    let
        n =
            reduce start

        nList =
            acc ++ [ n ]
    in
    if n == end then
        nList

    else
        getRange n end reduce acc


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
                    getRange curY (curY - instruction.length) (\v -> v - 1) []

                ( coords, currentCoords ) =
                    List.foldl
                        (\nextY ( coordSet, _ ) ->
                            let
                                latest =
                                    ( nextY, curX )
                            in
                            ( Set.insert latest coordSet, latest )
                        )
                        ( Set.empty, acc.currentCoords )
                        range
            in
            { coords = coords, currentCoords = currentCoords }

        Down ->
            let
                range =
                    getRange curY (curY - instruction.length) (\v -> v + 1) []

                ( coords, currentCoords ) =
                    List.foldl
                        (\nextY ( coordSet, _ ) ->
                            let
                                latest =
                                    ( nextY, curX )
                            in
                            ( Set.insert latest coordSet, latest )
                        )
                        ( Set.empty, acc.currentCoords )
                        range
            in
            { coords = coords, currentCoords = currentCoords }

        Left ->
            let
                range =
                    getRange curX (curX - instruction.length) (\v -> v - 1) []

                ( coords, currentCoords ) =
                    List.foldl
                        (\nextX ( coordSet, _ ) ->
                            let
                                latest =
                                    ( curY, nextX )
                            in
                            ( Set.insert latest coordSet, latest )
                        )
                        ( Set.empty, acc.currentCoords )
                        range
            in
            { coords = coords, currentCoords = currentCoords }

        Right ->
            let
                range =
                    getRange curX (curX - instruction.length) (\v -> v + 1) []

                ( coords, currentCoords ) =
                    List.foldl
                        (\nextX ( coordSet, _ ) ->
                            let
                                latest =
                                    ( curY, nextX )
                            in
                            ( Set.insert latest coordSet, latest )
                        )
                        ( Set.empty, acc.currentCoords )
                        range
            in
            { coords = coords, currentCoords = currentCoords }


type alias InstructionGenerator =
    { currentCoords : Coords
    , coords : Set Coords
    }


instructionsToCoords : List Instruction -> Set Coords
instructionsToCoords instructions =
    List.foldl
        instructionToCoords
        { coords = Set.empty, currentCoords = centralPort }
        instructions
        |> .coords


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

        l =
            Debug.log "wires = " wires
    in
    Result.Ok "wip"


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
