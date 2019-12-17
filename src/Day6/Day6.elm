module Day6 exposing (main)

import Array
import Basics.Extra exposing (flip)
import Browser exposing (element)
import Html as H
import List.Extra as ListX
import Maybe.Extra as MaybeX
import Platform exposing (Program)
import Result.Extra as ResultX
import Set exposing (Set)


type alias Orbit =
    { parent : String
    , child : String
    }


sliceFromIndex : Int -> List a -> List a
sliceFromIndex idx list =
    ListX.indexedFoldl
        (\idx_ item all ->
            if idx_ >= idx then
                all ++ [ item ]

            else
                all
        )
        []
        list


getChildren : List Orbit -> String -> List String
getChildren orbits parent =
    List.filter
        (.parent >> (==) parent)
        orbits
        |> List.map .child


planetVar =
    { start = Char.isUpper
    , inner = Char.isUpper
    , reserved = Set.fromList [ ")" ]
    }


parseLine : String -> Result String Orbit
parseLine input =
    Result.map2
        Orbit
        (String.split ")" input |> List.head |> Result.fromMaybe "No parent found")
        (String.split ")" input |> ListX.last |> Result.fromMaybe "No child found")


inputToOrbitDict : String -> Result String ( List Orbit, String )
inputToOrbitDict =
    String.split "\n"
        >> List.map parseLine
        >> ResultX.combine
        >> Result.andThen
            (\orbits ->
                List.filter
                    (\orbit -> orbit.parent == "COM")
                    orbits
                    |> List.head
                    |> Maybe.map .child
                    |> Result.fromMaybe "No COM found"
                    |> Result.map (\sun -> ( orbits, sun ))
            )


buildSolarSystem : List Orbit -> String -> Node
buildSolarSystem orbitList planetTag =
    let
        children =
            getChildren orbitList planetTag
                |> List.map (buildSolarSystem orbitList)
    in
    Planet planetTag children


allPlanets : List Orbit -> List String
allPlanets =
    List.foldr
        (\orbit allOrbits -> allOrbits ++ [ orbit.parent, orbit.child ])
        []
        >> Set.fromList
        >> Set.toList


type Node
    = Planet String (List Node)


getNumberOfOrbits : String -> Int -> List String -> Node -> Maybe ( Int, List String )
getNumberOfOrbits target curDistance path node =
    case node of
        Planet nameTag children ->
            let
                currentPath =
                    path ++ [ nameTag ]
            in
            if nameTag == target then
                Just ( curDistance, currentPath )

            else
                List.map
                    (getNumberOfOrbits target (curDistance + 1) currentPath)
                    children
                    |> List.filter MaybeX.isJust
                    |> List.head
                    |> MaybeX.join


addPlanet : String -> String -> Node -> Node
addPlanet name targetParent node =
    case node of
        Planet nameTag children ->
            if nameTag == targetParent then
                Planet nameTag (Planet name [] :: children)

            else
                Planet
                    nameTag
                    (List.map
                        (addPlanet name targetParent)
                        children
                    )


type alias Problem =
    { input : String
    , part : Int
    }


type alias Solution =
    Result String String


closestIntersect : List String -> List String -> Maybe String
closestIntersect pathA pathB =
    let
        pathBSet =
            Set.fromList pathB
    in
    List.foldl
        (\pathAPlanet found ->
            if Set.member pathAPlanet pathBSet then
                Just pathAPlanet

            else
                found
        )
        Nothing
        pathA


distanceToSanta : Node -> Result String String
distanceToSanta solarSystem =
    let
        pathResults =
            Maybe.map2
                Tuple.pair
                (getNumberOfOrbits "YOU" 0 [] solarSystem |> Maybe.map Tuple.second)
                (getNumberOfOrbits "SAN" 0 [] solarSystem |> Maybe.map Tuple.second)

        closestIntersectResult =
            pathResults
                |> Maybe.andThen (\( a, b ) -> closestIntersect a b)
    in
    Maybe.map2
        (\intersect ( pathA, pathB ) ->
            Maybe.map2
                (\idxA idxB ->
                    sliceFromIndex idxA pathA ++ sliceFromIndex idxB pathB
                )
                (ListX.elemIndex intersect pathA)
                (ListX.elemIndex intersect pathB)
        )
        closestIntersectResult
        pathResults
        |> MaybeX.join
        -- remove duplicate
        |> Maybe.map ListX.unique
        -- don't count the initial orbits
        |> Maybe.map (List.length >> flip (-) 3 >> String.fromInt)
        |> Result.fromMaybe "Could not find distance"


partTwo : String -> Solution
partTwo input =
    inputToOrbitDict input
        |> Result.map (\( allOrbits, _ ) -> buildSolarSystem allOrbits "COM")
        |> Result.andThen distanceToSanta


partOne : String -> Solution
partOne input =
    let
        result =
            inputToOrbitDict input
                |> Result.map
                    (\( allOrbits, sun ) ->
                        let
                            solarSystem =
                                buildSolarSystem allOrbits "COM"

                            planets =
                                allPlanets allOrbits
                        in
                        List.foldr
                            (\planetTag totalDist ->
                                let
                                    distResult =
                                        getNumberOfOrbits planetTag 0 [] solarSystem
                                            |> Result.fromMaybe "distance not found"
                                in
                                Result.map
                                    (Tuple.first >> (+) totalDist)
                                    distResult
                                    |> Result.withDefault 1000000000000
                            )
                            0
                            planets
                    )
    in
    Result.map String.fromInt result


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
