module Day1 exposing (main)

import Array exposing (Array)
import Basics.Extra exposing (flip)
import Browser exposing (element)
import Html as H
import List.Extra as ListX
import Maybe.Extra as MaybeX
import Platform exposing (Program)
import Result.Extra as ResultX
import Set


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
    | JumpIfTrue JumpIfTrueOperands
    | JumpIfFalse JumpIfFalseOperands
    | IsLessThan IsLessThanOperands
    | IsEqualTo IsEqualToOperands
    | End


type Mode a
    = Immediate a
    | Positional a


type alias IsLessThanOperands =
    { x : Mode Int
    , y : Mode Int
    , pos : Mode Int
    }


type alias IsEqualToOperands =
    { x : Mode Int
    , y : Mode Int
    , pos : Mode Int
    }


type alias JumpIfFalseOperands =
    { shouldJump : Mode Int
    , jumpTo : Mode Int
    }


type alias JumpIfTrueOperands =
    { shouldJump : Mode Int
    , jumpTo : Mode Int
    }


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


type alias OperationResult =
    { opcodes : Array Int
    , nextInputIdx : Int
    , output : List Int
    , jumpTo : Maybe Int
    }


debugOpcodes : Int -> Array Int -> String
debugOpcodes position opcodes =
    let
        stringified =
            Array.map String.fromInt opcodes

        annotated =
            Maybe.map
                (\problemChild -> Array.set position ("**" ++ problemChild) stringified)
                (Array.get position stringified)
    in
    annotated
        |> Maybe.map Array.toList
        |> Maybe.map (List.intersperse ",")
        |> Maybe.map (List.foldr (++) "")
        |> Maybe.withDefault
            ("Unknown error position "
                ++ String.fromInt position
            )


resolveOperation : Int -> Array Int -> ( Int, Array Int ) -> List Int -> Operation -> Result String OperationResult
resolveOperation currentPlace array ( inputIdx, inputs ) outputs operation =
    let
        position =
            String.fromInt currentPlace
    in
    case operation of
        IsLessThan isLessThanOperands ->
            Maybe.map3
                (\x y pos ->
                    if x < y then
                        ( pos, 1 )

                    else
                        ( pos, 0 )
                )
                (resolveOperand isLessThanOperands.x array)
                (resolveOperand isLessThanOperands.y array)
                (resolveOperand isLessThanOperands.pos array)
                |> Result.fromMaybe ("failed to resolve IsLessThan operands at position " ++ position ++ debugOpcodes currentPlace array)
                |> Result.map
                    (\( pos, value ) ->
                        OperationResult
                            (Array.set pos value array)
                            inputIdx
                            outputs
                            Nothing
                    )

        IsEqualTo isEqualToOperands ->
            Maybe.map3
                (\x y pos ->
                    if x == y then
                        ( pos, 1 )

                    else
                        ( pos, 0 )
                )
                (resolveOperand isEqualToOperands.x array)
                (resolveOperand isEqualToOperands.y array)
                (resolveOperand isEqualToOperands.pos array)
                |> Result.fromMaybe ("failed to resolve IsEqualTo operands at position " ++ position ++ debugOpcodes currentPlace array)
                |> Result.map
                    (\( pos, value ) ->
                        OperationResult
                            (Array.set pos value array)
                            inputIdx
                            outputs
                            Nothing
                    )

        JumpIfTrue jumpIfTrueOperands ->
            Maybe.map2
                (\shouldJump jumpTo ->
                    if shouldJump == 0 then
                        Nothing

                    else
                        Just jumpTo
                )
                (resolveOperand jumpIfTrueOperands.shouldJump array)
                (resolveOperand jumpIfTrueOperands.jumpTo array)
                |> Result.fromMaybe ("failed to resolve jumpto operands at position " ++ position ++ debugOpcodes currentPlace array)
                |> Result.map
                    (\jumpTo ->
                        OperationResult
                            array
                            inputIdx
                            outputs
                            jumpTo
                    )

        JumpIfFalse jumpIfFalseOperands ->
            Maybe.map2
                (\shouldJump jumpTo ->
                    if shouldJump == 0 then
                        Just jumpTo

                    else
                        Nothing
                )
                (resolveOperand jumpIfFalseOperands.shouldJump array)
                (resolveOperand jumpIfFalseOperands.jumpTo array)
                |> Result.fromMaybe ("failed to resolve JumpIfFalse operands at position " ++ position ++ debugOpcodes currentPlace array)
                |> Result.map
                    (\jumpTo ->
                        OperationResult
                            array
                            inputIdx
                            outputs
                            jumpTo
                    )

        MoveInput moveInputOperands ->
            Maybe.map2
                (\input pos -> Array.set pos input array)
                (Array.get inputIdx inputs)
                (resolveOperand moveInputOperands.pos array)
                |> Result.fromMaybe ("failed to resolve move input operands at position " ++ position ++ debugOpcodes currentPlace array)
                |> Result.map
                    (\opcodes ->
                        OperationResult
                            opcodes
                            (inputIdx + 1)
                            outputs
                            Nothing
                    )

        OutputValue outputValueOperands ->
            Maybe.map
                (\outputValue -> ( array, outputValue ))
                (resolveOperand outputValueOperands.value array)
                |> Result.fromMaybe ("failed to resolve move value operands at position " ++ position ++ debugOpcodes currentPlace array)
                |> Result.map
                    (\( opcodes, outputValue ) ->
                        OperationResult
                            opcodes
                            inputIdx
                            (outputs ++ [ outputValue ])
                            Nothing
                    )

        Add addOperands ->
            Maybe.map3
                (\x y pos -> Array.set pos (x + y) array)
                (resolveOperand addOperands.x array)
                (resolveOperand addOperands.y array)
                (resolveOperand addOperands.pos array)
                |> Result.fromMaybe ("Failed to resolve add operands at position " ++ position ++ debugOpcodes currentPlace array)
                |> Result.map
                    (\opcodes ->
                        OperationResult
                            opcodes
                            inputIdx
                            outputs
                            Nothing
                    )

        Multiply multiplyOperands ->
            Maybe.map3
                (\x y pos -> Array.set pos (x * y) array)
                (resolveOperand multiplyOperands.x array)
                (resolveOperand multiplyOperands.y array)
                (resolveOperand multiplyOperands.pos array)
                |> Result.fromMaybe ("Failed to resolve multiply operands at position " ++ position ++ debugOpcodes currentPlace array)
                |> Result.map
                    (\opcodes ->
                        OperationResult
                            opcodes
                            inputIdx
                            outputs
                            Nothing
                    )

        _ ->
            Result.Err ("Unresolvable operation at: " ++ position ++ debugOpcodes currentPlace array)


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

        5 ->
            Maybe.map2
                JumpIfTrueOperands
                (Array.get (currentPlace + 1) array |> Maybe.map mapFirst)
                (Array.get (currentPlace + 2) array |> Maybe.map mapSecond)
                |> Maybe.map (JumpIfTrue >> Tuple.pair 2)

        6 ->
            Maybe.map2
                JumpIfFalseOperands
                (Array.get (currentPlace + 1) array |> Maybe.map mapFirst)
                (Array.get (currentPlace + 2) array |> Maybe.map mapSecond)
                |> Maybe.map (JumpIfFalse >> Tuple.pair 2)

        7 ->
            Maybe.map3
                IsLessThanOperands
                (Array.get (currentPlace + 1) array |> Maybe.map mapFirst)
                (Array.get (currentPlace + 2) array |> Maybe.map mapSecond)
                (Array.get (currentPlace + 3) array |> Maybe.map Immediate)
                |> Maybe.map (IsLessThan >> Tuple.pair 3)

        8 ->
            Maybe.map3
                IsEqualToOperands
                (Array.get (currentPlace + 1) array |> Maybe.map mapFirst)
                (Array.get (currentPlace + 2) array |> Maybe.map mapSecond)
                (Array.get (currentPlace + 3) array |> Maybe.map Immediate)
                |> Maybe.map (IsEqualTo >> Tuple.pair 3)

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


readOpcodeArray : Int -> ( Int, Array Int ) -> List Int -> Array Int -> Result String (List Int)
readOpcodeArray currentPlace ( inputIdx, inputs ) outputs array =
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
                    Result.Ok outputs

                Just ( readLength, operation ) ->
                    resolveOperation currentPlace array ( inputIdx, inputs ) outputs operation
                        |> Result.andThen
                            (\result ->
                                let
                                    nextPos =
                                        Maybe.map
                                            (\val -> val)
                                            result.jumpTo
                                            |> Maybe.withDefault (currentPlace + 1 + readLength)
                                in
                                readOpcodeArray
                                    nextPos
                                    ( result.nextInputIdx, inputs )
                                    result.output
                                    result.opcodes
                            )

                Nothing ->
                    Result.Err ("Unknown operation found at: " ++ String.fromInt currentPlace ++ debugOpcodes currentPlace array)

        Nothing ->
            Result.Err ("Ran out of index without End operation" ++ debugOpcodes currentPlace array)


pipeOutput : Array Int -> Int -> Int -> Result String (List Int)
pipeOutput input phaseSetting prevOutput =
    readOpcodeArray 0 ( 0, Array.fromList [ phaseSetting, prevOutput ] ) [] input


getOutput : List Int -> Result String Int
getOutput =
    ListX.last
        >> Result.fromMaybe "program did not emit output"


inputSet : Inputs
inputSet =
    { a = 0
    , b = 0
    , c = 0
    , d = 0
    , e = 0
    }


nextPermutation : Inputs -> Result String Inputs
nextPermutation inputs =
    let
        prospectNext =
            if inputs.a == 4 then
                if inputs.b == 4 then
                    if inputs.c == 4 then
                        if inputs.d == 4 then
                            if inputs.e == 4 then
                                Result.Err "Reached end"

                            else
                                Result.Ok { inputs | e = inputs.e + 1, d = 0, c = 0, b = 0, a = 0 }

                        else
                            Result.Ok { inputs | d = inputs.d + 1, c = 0, b = 0, a = 0 }

                    else
                        Result.Ok { inputs | c = inputs.c + 1, b = 0, a = 0 }

                else
                    Result.Ok { inputs | b = inputs.b + 1, a = 0 }

            else
                Result.Ok { inputs | a = inputs.a + 1 }
    in
    -- if any match then we need to skip
    Result.andThen
        (\n ->
            let
                list =
                    [ n.a, n.b, n.c, n.d, n.e ]
            in
            if (Set.fromList list |> Set.size) /= List.length list then
                nextPermutation n

            else
                Result.Ok n
        )
        prospectNext


type alias Inputs =
    { a : Int
    , b : Int
    , c : Int
    , d : Int
    , e : Int
    }


processSolution : Array Int -> Inputs -> Solution
processSolution opcodes computerInputs =
    readOpcodeArray 0 ( 0, Array.fromList [ computerInputs.a, 0 ] ) [] opcodes
        |> Result.andThen getOutput
        |> Result.andThen (pipeOutput opcodes computerInputs.b)
        |> Result.andThen getOutput
        |> Result.andThen (pipeOutput opcodes computerInputs.c)
        |> Result.andThen getOutput
        |> Result.andThen (pipeOutput opcodes computerInputs.d)
        |> Result.andThen getOutput
        |> Result.andThen (pipeOutput opcodes computerInputs.e)
        |> Result.andThen getOutput
        |> Result.map String.fromInt


processAllSolutions : Array Int -> Inputs -> List String -> List String
processAllSolutions opcodes inputs results =
    let
        nextResults =
            Result.map
                (\solution -> solution :: results)
                (processSolution opcodes (Debug.log "PROCESSING FOR" inputs) |> Debug.log "PROCESSING RESULTS")
                |> Result.withDefault results

        nextInputs =
            nextPermutation inputs
    in
    case nextInputs of
        Result.Ok next ->
            processAllSolutions opcodes next nextResults

        Result.Err _ ->
            results


partOne : String -> Solution
partOne input =
    -- 998120 is wrong
    -- 43210
    let
        -- 1,0,4,3,2
        debug =
            input
                |> inputToArray
                |> Result.map
                    (\opcodes ->
                        processSolution opcodes { a = 0, b = 1, c = 2, d = 3, e = 4 }
                    )
    in
    input
        |> inputToArray
        |> Result.map
            (\opcodes ->
                processAllSolutions opcodes inputSet []
            )
        |> Result.map (List.map (String.toInt >> Maybe.withDefault -1))
        |> Result.map
            (List.foldr
                (\cur champ ->
                    if cur > champ then
                        cur

                    else
                        champ
                )
                0
            )
        |> Result.map String.fromInt


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
