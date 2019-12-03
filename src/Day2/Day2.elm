module Day2 exposing (main)

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


inputToArray : String -> Result String (Array Int)
inputToArray =
    String.split ","
        >> List.map String.toInt
        >> List.map (Result.fromMaybe "Could not parse input")
        >> ResultX.combine
        >> Result.map Array.fromList


type alias Operands =
    { x : Int
    , y : Int
    , pos : Int
    }


prepareInput : Array Int -> Array Int
prepareInput =
    -- replace position 1 with the value 12 and replace position 2 with the value 2.
    Array.set 1 12
        >> Array.set 2 2


resolveOperands : Int -> Array Int -> Operands -> (Int -> Int -> Int) -> Result String (Array Int)
resolveOperands currentPlace array operands operation =
    let
        resultValue =
            operation operands.x operands.y

        resultArray =
            Array.set operands.pos resultValue array
    in
    Result.Ok resultArray


valueFromPos : Int -> Array Int -> Maybe Int
valueFromPos pos array =
    Array.get pos array
        |> Maybe.andThen (flip Array.get <| array)


readArray : Int -> Array Int -> Result String (Array Int)
readArray currentPlace array =
    let
        mCurrentOp =
            Array.get currentPlace array

        mOperands =
            Maybe.map3
                Operands
                (valueFromPos (currentPlace + 1) array)
                (valueFromPos (currentPlace + 2) array)
                (Array.get (currentPlace + 3) array)

        next =
            currentPlace + 4
    in
    case ( mCurrentOp, mOperands ) of
        ( Just currentOp, Just operands ) ->
            case currentOp of
                1 ->
                    resolveOperands currentPlace array operands (+)
                        |> Result.andThen (readArray next)

                2 ->
                    resolveOperands currentPlace array operands (*)
                        |> Result.andThen (readArray next)

                99 ->
                    Result.Ok array

                _ ->
                    Result.Err "Unknown operator"

        ( Just currentOp, Nothing ) ->
            case currentOp of
                99 ->
                    Result.Ok array

                _ ->
                    Result.Err "out of index"

        -- no more operations
        _ ->
            Result.Err "Out of index"


partOne : String -> Solution
partOne input =
    inputToArray input
        |> Result.map prepareInput
        |> Result.andThen (readArray 0)
        |> Result.map Array.toList
        |> Result.map (List.map String.fromInt)
        |> Result.map (List.intersperse ",")
        |> Result.map (List.foldr (++) "")


solve : Problem -> Solution
solve problem =
    case problem.part of
        0 ->
            partOne problem.input

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
