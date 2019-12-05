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
    { x : Int
    , y : Int
    }


centralPort : Coords
centralPort =
    Coords 500 500


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


instructionToCoords : Instruction -> InstructionGenerator -> InstructionGenerator
instructionToCoords instruction acc =
    acc


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
