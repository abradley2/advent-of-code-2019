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


prepareInput : InputPrefix -> Array Int -> Array Int
prepareInput prefix =
    -- replace position 1 with the value 12 and replace position 2 with the value 2.
    Array.set 1 prefix.noun
        >> Array.set 2 prefix.verb


type alias InputPrefix =
    { noun : Int
    , verb : Int
    }


incrementInputPrefix : InputPrefix -> Result String InputPrefix
incrementInputPrefix prefix =
    case prefix.verb of
        99 ->
            let
                next =
                    { noun = prefix.noun + 1, verb = 0 }
            in
            if next.noun == 99 then
                Result.Err "Exceeded last possibility, (99, 99)"

            else
                Result.Ok next

        _ ->
            Result.Ok { noun = prefix.noun, verb = prefix.verb + 1 }


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
        |> Result.map (prepareInput (InputPrefix 12 2))
        |> Result.andThen (readArray 0)
        |> Result.map Array.toList
        |> Result.map (List.map String.fromInt)
        |> Result.map (List.intersperse ",")
        |> Result.map (List.foldr (++) "")


partTwo : String -> InputPrefix -> Solution
partTwo input inputPrefix =
    let
        result =
            inputToArray input
                |> Result.map (prepareInput inputPrefix)
                |> Result.andThen (readArray 0)
                |> Result.map Array.toList
                |> Result.map (List.map String.fromInt)
                |> Result.andThen (List.head >> Result.fromMaybe "No result")
    in
    case result of
        Result.Ok solved ->
            if solved == "19690720" then
                Result.Ok (String.fromInt <| 100 * inputPrefix.noun + inputPrefix.verb)

            else
                Result.andThen
                    (partTwo input)
                    (incrementInputPrefix inputPrefix)

        Result.Err _ ->
            Result.andThen
                (partTwo input)
                (incrementInputPrefix inputPrefix)


solve : Problem -> Solution
solve problem =
    case problem.part of
        -- 1 ->
        --     partOne problem.input
        2 ->
            partTwo problem.input { noun = 0, verb = 0 }

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
