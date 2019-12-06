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


type alias Solution a =
    Result String (H.Html a)


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
    ( 8, 1 )


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

                ( coords, currentCoords ) =
                    List.foldl
                        (\nextY ( coordSet, _ ) ->
                            let
                                latest =
                                    ( curX, nextY )
                            in
                            ( Set.insert latest coordSet, latest )
                        )
                        ( Set.empty, acc.currentCoords )
                        range
            in
            { coords = Set.union coords acc.coords, currentCoords = currentCoords }

        Down ->
            let
                range =
                    getRange curY (curY + instruction.length)

                ( coords, currentCoords ) =
                    List.foldl
                        (\nextY ( coordSet, _ ) ->
                            let
                                latest =
                                    ( curX, nextY )
                            in
                            ( Set.insert latest coordSet, latest )
                        )
                        ( Set.empty, acc.currentCoords )
                        range
            in
            { coords = Set.union coords acc.coords, currentCoords = currentCoords }

        Left ->
            let
                range =
                    getRange curX (curX - instruction.length)

                ( coords, currentCoords ) =
                    List.foldl
                        (\nextX ( coordSet, _ ) ->
                            let
                                latest =
                                    ( nextX, curY )
                            in
                            ( Set.insert latest coordSet, latest )
                        )
                        ( Set.empty, acc.currentCoords )
                        range
            in
            { coords = Set.union coords acc.coords, currentCoords = currentCoords }

        Right ->
            let
                range =
                    getRange curX (curX - instruction.length)

                ( coords, currentCoords ) =
                    List.foldl
                        (\nextX ( coordSet, _ ) ->
                            let
                                latest =
                                    ( nextX, curY )
                            in
                            ( Set.insert latest coordSet, latest )
                        )
                        ( Set.empty, acc.currentCoords )
                        range
            in
            { coords = Set.union coords acc.coords, currentCoords = currentCoords }


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


partOne : String -> Solution Msg
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
                |> Result.map (\( wireA, wireB ) -> Set.toList wireA ++ Set.toList wireB)
                |> Result.map (coordsToRender >> H.div [
                    A.style "margin-left" "200px"
                    , A.style "margin-top" "200px"
                    ])

        -- |> Result.map
        --     (\( wireA, wireB ) ->
        --         Set.intersect (Debug.log "Wire A" wireA) (Debug.log "Wire B" wireB)
        --     )
        -- |> Result.map Set.toList
        -- |> Result.map
        --     (List.foldl
        --         (\sut champion ->
        --             if Debug.log "sut" sut < champion && sut /= ( 0, 0 ) then
        --                 sut
        --             else
        --                 champion
        --         )
        --         ( 0, 0 )
        --     )
        -- |> Result.map (\( x, y ) -> abs x + abs y |> String.fromInt)
        -- |> Result.map (\( x, y ) -> "(" ++ String.fromInt x ++ "," ++ String.fromInt y ++ ")")
    in
    wires
        |> Result.mapError (\_ -> "could not parse input")


coordsToRender : List Coords -> List (H.Html Msg)
coordsToRender coords =
    List.map
        (\( x, y ) ->
            H.div
                [ A.style "transform" <| "translateY(" ++ String.fromInt (y * 20) ++ "px)" ++ " translateX(" ++ String.fromInt (x * 20) ++ "px)"
                , A.style "position" "absolute"
                , A.style "font-family" "courier"
                , A.style "width" "20px"
                , A.style "width" "20px"
                ]
                [ H.text "#"
                ]
        )
        (Debug.log "RENDER COORDS" coords)


solve : Problem -> Solution Msg
solve problem =
    case problem.part of
        1 ->
            partOne problem.input

        _ ->
            Result.Err
                ("No solution for problem number = "
                    ++ String.fromInt problem.part
                )


main : Program Problem (Solution Msg) Msg
main =
    element
        { init = solve >> (\model -> ( model, Cmd.none ))
        , view = Result.withDefault (H.div [A.style "padding-left" "200px"] [ H.text "Error" ])
        , update = update
        , subscriptions = \_ -> Sub.none
        }


update : Msg -> Solution Msg -> ( Solution Msg, Cmd Msg )
update _ model =
    ( model, Cmd.none )


type Msg
    = NoOp
