module Day3 exposing (main)

import Array exposing (Array)
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


setY : Coords -> Int -> Coords
setY coords val =
    Tuple.mapSecond (\_ -> val) coords


setX : Coords -> Int -> Coords
setX coords val =
    Tuple.mapFirst (\_ -> val) coords


centralPort : Coords
centralPort =
    ( 0, 0 )


getRange : Int -> Int -> List Int
getRange start end =
    if start > end then
        List.range end start |> List.reverse

    else
        List.range start end


type alias CoordsListResult =
    { -- all the coords an instruction resulted in
      coordsList : List Coords

    -- the end coordinate used to process the next instruction
    , endCoords : Coords

    -- the end wire distance traveled used to process the next instruction
    , endDistance : Int

    -- an updated map of instersect coords to wire distance as a result
    -- of processing the instruction
    , intersectDistanceMap : Dict Coords Int
    }


instructionToCoordsList : Set Coords -> ( Coords, Int ) -> Instruction -> CoordsListResult
instructionToCoordsList knownIntersections ( startCoords, startDistance ) instruction =
    let
        ( x, y ) =
            startCoords

        ( range, setter ) =
            case instruction.direction of
                Up ->
                    ( getRange y (y - instruction.length)
                    , setY startCoords
                    )

                Down ->
                    ( getRange y (y + instruction.length)
                    , setY startCoords
                    )

                Left ->
                    ( getRange x (x - instruction.length)
                    , setX startCoords
                    )

                Right ->
                    ( getRange x (x + instruction.length)
                    , setX startCoords
                    )
    in
    List.foldl
        (\newCoord acc ->
            let
                newCoords =
                    setter newCoord

                endDistance =
                    acc.endDistance + 1

                intersectDistanceMap =
                    if Set.member newCoords knownIntersections then
                        Dict.insert newCoords endDistance acc.intersectDistanceMap

                    else
                        acc.intersectDistanceMap
            in
            { coordsList = acc.coordsList ++ [ newCoords ]
            , endDistance = endDistance
            , endCoords = newCoords
            , intersectDistanceMap = intersectDistanceMap
            }
        )
        { coordsList = []
        , endDistance = startDistance
        , endCoords = startCoords
        , intersectDistanceMap = Dict.empty
        }
        range


instructionsListToCoords : List Instruction -> Set Coords
instructionsListToCoords =
    List.foldl
        (\instruction ( nextSet, nextCoord ) ->
            let
                coordsListResult =
                    instructionToCoordsList Set.empty ( nextCoord, 0 ) instruction
            in
            ( Set.fromList coordsListResult.coordsList
            , coordsListResult.endCoords
            )
        )
        ( Set.empty, centralPort )
        >> Tuple.first


coordsToDist : Coords -> Int
coordsToDist ( x, y ) =
    abs x + abs y


getIntersectionsFromInput : String -> Result String (Set Coords)
getIntersectionsFromInput input =
    Result.map2
        Tuple.pair
        (String.split "\n" input
            |> List.head
            |> Result.fromMaybe "Could not find line for first wire"
            |> Result.andThen lineToInstructions
            |> Result.map instructionsListToCoords
        )
        (String.split "\n" input
            |> ListX.last
            |> Result.fromMaybe "Could not find line for second wirte"
            |> Result.andThen lineToInstructions
            |> Result.map instructionsListToCoords
        )
        |> Result.map
            (\( wireA, wireB ) ->
                Set.intersect wireA wireB
            )


partTwo : String -> Solution
partTwo input =
    let
        -- first we need to get all intersections from our input
        intersections =
            getIntersectionsFromInput input

        -- we then need to go back and get two instruction lists
        instructionLists =
            Result.map2
                Tuple.pair
                (String.split "\n" input
                    |> List.head
                    |> Result.fromMaybe "Could not find line for first wire"
                    |> Result.andThen lineToInstructions
                )
                (String.split "\n" input
                    |> ListX.last
                    |> Result.fromMaybe "Could not find line for second wirte"
                    |> Result.andThen lineToInstructions
                )

        -- now we can process the instructions with known intersections to get
        -- our intersetDistanceMaps
    in
    Result.Err "not ready yet"


partOne : String -> Solution
partOne input =
    let
        wires =
            getIntersectionsFromInput input
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


lineToInstructions : String -> Result String (List Instruction)
lineToInstructions input =
    input
        |> String.split ","
        |> List.map (Parser.run parseInstruction >> Result.mapError (\_ -> "Failed to parse instruction line"))
        |> ResultX.combine
