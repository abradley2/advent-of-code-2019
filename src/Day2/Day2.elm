module Day2 exposing (main)

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


partOne : String -> Solution
partOne input =
    Result.Err input



{-
   99 = finished
   1 adds together numbers read from two positions and stores the result in a third position

   The three integers immediately after the opcode tell you these three positions
   - first two =  the two positions to read from
   - third position to store output

   1, 10, 20, 30

   Add x(10) + y(10) and store at 30

   opcode 2 does the same but multiplies instead of adding

   When you see an opcode, move to the next one by stepping forward 4 positions

-}


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
