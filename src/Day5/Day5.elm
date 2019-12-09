module Day5 exposing (main)

import Array exposing (Array)
import Basics.Extra exposing (flip)
import Browser exposing (element)
import Html as H
import Platform exposing (Program)
import Result.Extra as ResultX


type alias Problem =
    { input : String
    , part : Int
    }


type alias Solution =
    Result String String


type Operation
    = Add AddOperands
    | Multiply MultiplyOperands
    | ToggleMode Mode
    | End


type Mode
    = Unknown
    | Immediate
    | Posotional


type alias AddOperands =
    { x : Int
    , y : Int
    , pos : Int
    }


type alias MultiplyOperands =
    { x : Int
    , y : Int
    , pos : Int
    }


resolveOperation : Int -> Array Int -> Operation -> Result String (Array Int)
resolveOperation currentPlace array operation =
    case operation of
        Add addOperands ->
            Array.set addOperands.pos (addOperands.x + addOperands.y) array
                |> Result.Ok

        Multiply multiplyOperands ->
            Array.set multiplyOperands.pos (multiplyOperands.x + multiplyOperands.y) array
                |> Result.Ok

        _ ->
            Result.Err ("Unresolvable operation at: " ++ String.fromInt currentPlace)


valueFromPos : Int -> Array Int -> Maybe Int
valueFromPos pos opcodeArray =
    Array.get pos opcodeArray
        |> Maybe.andThen (flip Array.get <| opcodeArray)


readOpcodeArray : Int -> Array Int -> Result String (Array Int)
readOpcodeArray currentPlace array =
    let
        mCurrentOp =
            Array.get currentPlace array

        mOperation =
            mCurrentOp
                |> Maybe.andThen
                    (\op ->
                        case op of
                            1 ->
                                Maybe.map3
                                    AddOperands
                                    (valueFromPos (currentPlace + 1) array)
                                    (valueFromPos (currentPlace + 2) array)
                                    (Array.get (currentPlace + 3) array)
                                    |> Maybe.map (Add >> Tuple.pair 3)

                            2 ->
                                Maybe.map3
                                    MultiplyOperands
                                    (valueFromPos (currentPlace + 1) array)
                                    (valueFromPos (currentPlace + 2) array)
                                    (Array.get (currentPlace + 3) array)
                                    |> Maybe.map (Multiply >> Tuple.pair 3)

                            99 ->
                                Just ( 0, End )

                            _ ->
                                Nothing
                    )
    in
    case mCurrentOp of
        Just _ ->
            case mOperation of
                Just ( _, End ) ->
                    Result.Ok array

                Just ( readLength, operation ) ->
                    let
                        -- the next read position is 1 + how many operands we read
                        next =
                            currentPlace + 1 + readLength
                    in
                    resolveOperation currentPlace array operation
                        |> Result.andThen (readOpcodeArray next)

                Nothing ->
                    Result.Err ("Unknown operation found at: " ++ String.fromInt currentPlace)

        Nothing ->
            Result.Err "Ran out of index without End operation"


partOne : String -> Solution
partOne input =
    Result.Err "No solution for part one"


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
