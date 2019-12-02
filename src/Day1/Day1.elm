module Day1 exposing (main)

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


computeFuel : Int -> Int
computeFuel mass =
    (toFloat mass / 3.0)
        |> floor
        |> (\v -> v - 2)
        |> max 0


computeExtraFuel : Int -> Int
computeExtraFuel inputFuel =
    let
        fuelForFuel =
            computeFuel inputFuel
    in
    if fuelForFuel == 0 then
        inputFuel

    else
        inputFuel + computeExtraFuel fuelForFuel


partOne : String -> Solution
partOne =
    -- split the input by lines
    String.split "\n"
        >> -- parse into integers
           List.map (String.toInt >> Result.fromMaybe "could not parse all inputs")
        >> -- validate input
           ResultX.combine
        >> -- compute fuel needed per each line of mass
           Result.map (List.map computeFuel)
        >> -- add it all up
           Result.map (List.foldl (+) 0)
        >> --format answer
           Result.map String.fromInt


partTwo : String -> Solution
partTwo =
    -- split the input by lines
    String.split "\n"
        >> -- parse into integers
           List.map (String.toInt >> Result.fromMaybe "could not parse all inputs")
        >> -- validate input
           ResultX.combine
        >> -- compute fuel needed per each line of mass
           Result.map (List.map computeFuel)
        >> -- compute fuel for fuel
           Result.map (List.map computeExtraFuel)
        >> -- add it all up
           Result.map (List.foldl (+) 0)
        >> --format answer
           Result.map String.fromInt


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
