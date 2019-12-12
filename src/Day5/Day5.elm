module Day5 exposing (main)

import Array exposing (Array)
import Basics.Extra exposing (flip)
import Browser exposing (element)
import Html as H
import Maybe.Extra as MaybeX
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
    | MoveInput MoveInputOperands
    | OutputValue OutputValueOperands
    | End



{-
   Opcode 3 takes a single integer as input and saves it to the position given by its only parameter.
   For example, the instruction 3,50 would take an input value and store it at address 50.

   Opcode 4 outputs the value of its only parameter. For example, the instruction 4,50 would output the value at address 50.
-}


type Mode a
    = Immediate a
    | Positional a


type alias MoveInputOperands =
    { pos : Mode Int
    }


type alias OutputValueOperands =
    { value : Mode Int
    }


type alias AddOperands =
    { x : Mode Int
    , y : Mode Int
    , pos : Mode Int
    }


type alias MultiplyOperands =
    { x : Mode Int
    , y : Mode Int
    , pos : Mode Int
    }


resolveOperand : Mode Int -> Array Int -> Maybe Int
resolveOperand op opcodeArray =
    case op of
        Positional val ->
            Array.get val opcodeArray

        Immediate val ->
            Just val


resolveOperation : Int -> Array Int -> Operation -> Result String (Array Int)
resolveOperation currentPlace array operation =
    let
        position =
            String.fromInt currentPlace
    in
    case operation of
        MoveInput moveInputOperands ->
            Maybe.map
                -- set = position value
                (\pos -> Array.set pos 1 array)
                (resolveOperand moveInputOperands.pos array)
                |> Result.fromMaybe ("failed to resolve move input operands at position " ++ position)

        OutputValue outputValueOperands ->
            Maybe.map
                (\posValue ->
                    let
                        curPlace =
                            Debug.log "Current place" currentPlace

                        outputValue =
                            Debug.log "OUTPUT$$$$" posValue
                    in
                    array
                )
                (resolveOperand outputValueOperands.value array)
                |> Result.fromMaybe ("failed to resolve move value operands at position " ++ position)

        Add addOperands ->
            Maybe.map3
                (\x y pos -> Array.set pos (x + y) array)
                (resolveOperand addOperands.x array)
                (resolveOperand addOperands.y array)
                (resolveOperand addOperands.pos array)
                |> Result.fromMaybe ("Failed to resolve add operands at position " ++ position)

        Multiply multiplyOperands ->
            Maybe.map3
                (\x y pos -> Array.set pos (x * y) array)
                (resolveOperand multiplyOperands.x array)
                (resolveOperand multiplyOperands.y array)
                (resolveOperand multiplyOperands.pos array)
                |> Result.fromMaybe ("Failed to resolve multiply operands at position " ++ position)

        _ ->
            Result.Err ("Unresolvable operation at: " ++ position)


valueFromPos : Int -> Array Int -> Maybe Int
valueFromPos pos opcodeArray =
    Array.get pos opcodeArray
        |> Maybe.andThen (flip Array.get <| opcodeArray)


type alias OpMap =
    Int -> Mode Int


toOpMap : Int -> Maybe OpMap
toOpMap val =
    case val of
        0 ->
            Just Positional

        1 ->
            Just Immediate

        _ ->
            Nothing


sliceList : Int -> Int -> List a -> List a
sliceList a b list =
    Array.fromList list
        |> Array.slice a b
        |> Array.toList


codeToOperation : Int -> Array Int -> OpMap -> OpMap -> OpMap -> Int -> Maybe ( Int, Operation )
codeToOperation currentPlace array mapFirst mapSecond mapThird op =
    case op of
        1 ->
            Maybe.map3
                AddOperands
                (Array.get (currentPlace + 1) array |> Maybe.map mapFirst)
                (Array.get (currentPlace + 2) array |> Maybe.map mapSecond)
                (Array.get (currentPlace + 3) array |> Maybe.map Immediate)
                |> Maybe.map (Add >> Tuple.pair 3)

        2 ->
            Maybe.map3
                MultiplyOperands
                (Array.get (currentPlace + 1) array |> Maybe.map mapFirst)
                (Array.get (currentPlace + 2) array |> Maybe.map mapSecond)
                (Array.get (currentPlace + 3) array |> Maybe.map Immediate)
                |> Maybe.map (Multiply >> Tuple.pair 3)

        3 ->
            Maybe.map
                MoveInputOperands
                (Array.get (currentPlace + 1) array |> Maybe.map Immediate)
                |> Maybe.map (MoveInput >> Tuple.pair 1)

        4 ->
            Maybe.map
                OutputValueOperands
                (Array.get (currentPlace + 1) array |> Maybe.map Positional)
                |> Maybe.map (OutputValue >> Tuple.pair 1)

        99 ->
            Just ( 0, End )

        _ ->
            let
                strList =
                    String.fromInt op
                        |> String.padLeft 5 '0'
                        |> String.toList
                        |> List.map String.fromChar

                newOp =
                    sliceList 3 5 strList |> List.foldr (++) "" |> String.toInt

                mapFirst_ =
                    Array.get 2 (Array.fromList strList)
                        |> Maybe.andThen String.toInt
                        |> Maybe.andThen toOpMap

                mapSecond_ =
                    Array.get 1 (Array.fromList strList)
                        |> Maybe.andThen String.toInt
                        |> Maybe.andThen toOpMap

                mapThird_ =
                    Array.get 0 (Array.fromList strList)
                        |> Maybe.andThen String.toInt
                        |> Maybe.andThen toOpMap
            in
            Maybe.map4
                (codeToOperation currentPlace array)
                mapFirst_
                mapSecond_
                mapThird_
                newOp
                |> MaybeX.join


readOpcodeArray : Int -> Array Int -> Result String (Array Int)
readOpcodeArray currentPlace array =
    let
        mCurrentOp =
            Array.get currentPlace array

        mOperation =
            mCurrentOp
                |> Maybe.andThen
                    (codeToOperation currentPlace array Positional Positional Immediate)
    in
    case mCurrentOp of
        Just _ ->
            case mOperation of
                Just ( _, End ) ->
                    Result.Ok array

                Just ( readLength, operation ) ->
                    resolveOperation currentPlace array operation
                        |> Result.andThen (readOpcodeArray (currentPlace + 1 + readLength))

                Nothing ->
                    Result.Err ("Unknown operation found at: " ++ String.fromInt currentPlace)

        Nothing ->
            Result.Err "Ran out of index without End operation"


partOne : String -> Solution
partOne input =
    let
        result =
            input
                |> inputToArray
                |> Result.map (readOpcodeArray 0)
                |> Debug.log "RESULT"
    in
    Result.Ok input


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


inputToArray : String -> Result String (Array Int)
inputToArray =
    String.split ","
        >> List.map String.toInt
        >> List.map (Result.fromMaybe "Could not parse input")
        >> ResultX.combine
        >> Result.map Array.fromList
