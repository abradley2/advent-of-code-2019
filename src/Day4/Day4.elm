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


isValidPassword : Int -> Bool
isValidPassword passNum =
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
                        Maybe.map
                            ((==) curDigit)
                            (Array.get (idx + 1) (Array.fromList digitList))
                            |> Maybe.withDefault False
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


partOne : String -> Solution
partOne input =
    let
        inputRange =
            List.range 246515 739105
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
