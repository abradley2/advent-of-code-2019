module Day4 exposing (main)

import Array
import Browser exposing (element)
import Html as H
import List.Extra as ListX
import Platform exposing (Program)
import Result.Extra as ResultX


type alias Problem =
    { input : String
    , part : Int
    }


type alias Solution =
    Result String String



{-

   It is a six-digit number.
   The value is within the range given in your puzzle input.
   Two adjacent digits are the same (like 22 in 122345).
   Going from left to right, the digits never decrease; they only ever increase or stay the same (like 111123 or 135679).


-}


isValidPassword : Bool -> Int -> Bool
isValidPassword allowThree passNum =
    let
        digitList =
            String.fromInt passNum |> String.split ""

        isSixDigit =
            passNum > 99999 && passNum < 999999

        hasDouble =
            ListX.indexedFoldl
                (\idx curDigit found ->
                    if found then
                        found

                    else
                        -- look ahead one and check if there is a match to determine if this is a "double"
                        (Maybe.map
                            ((==) curDigit)
                            (Array.get (idx + 1) (Array.fromList digitList))
                            |> Maybe.withDefault False
                        )
                            && (allowThree
                                    -- in the case of a double we need to check if the number an additional
                                    -- spot up, and the number on spot back is a match else it is a 3+ not a double
                                    || ((Maybe.map
                                            ((/=) curDigit)
                                            (Array.get (idx + 2) (Array.fromList digitList))
                                            |> Maybe.withDefault True
                                        )
                                            && (Maybe.map
                                                    ((/=) curDigit)
                                                    (Array.get (idx - 1) (Array.fromList digitList))
                                                    |> Maybe.withDefault True
                                               )
                                       )
                               )
                )
                False
                digitList

        noDecrease =
            ListX.indexedFoldl
                (\idx curDigit allIncrease ->
                    if allIncrease == False then
                        False

                    else
                        Maybe.map
                            (\val -> val >= curDigit)
                            (Array.get (idx + 1) (Array.fromList digitList))
                            |> Maybe.withDefault True
                )
                True
                digitList
    in
    isSixDigit && hasDouble && noDecrease


isValidPasswordPartOne : Int -> Bool
isValidPasswordPartOne =
    isValidPassword True


isValidPasswordPartTwo : Int -> Bool
isValidPasswordPartTwo =
    isValidPassword False


partOne : String -> Solution
partOne input =
    List.range 246515 739105
        |> List.filter isValidPasswordPartOne
        |> List.length
        |> String.fromInt
        |> Result.Ok


partTwo : String -> Solution
partTwo input =
    List.range 246515 739105
        |> List.filter isValidPasswordPartTwo
        |> List.length
        |> String.fromInt
        |> Result.Ok


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
