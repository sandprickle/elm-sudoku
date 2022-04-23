module Sudoku.Grid exposing (Cell(..), Coord, Grid, cellFromChar, fromString, getByCoord, getByIndex, toRowsList)

import Array exposing (Array)
import List.Extra
import Sudoku.Value as Value exposing (Value)


type Cell
    = Empty
    | Filled Value


cellFromChar : Char -> Cell
cellFromChar char =
    case Value.fromChar char of
        Just value ->
            Filled value

        Nothing ->
            Empty


type Grid
    = Grid (Array Cell)


getByCoord : Coord -> Grid -> Cell
getByCoord coord (Grid grid) =
    let
        index =
            coordToIndex coord
    in
    Array.get index grid |> Maybe.withDefault Empty


getByIndex : Int -> Grid -> Cell
getByIndex index (Grid grid) =
    Array.get (normalizeIndex index) grid
        |> Maybe.withDefault Empty


type alias Coord =
    { x : Int, y : Int }


indexToCoord : Int -> Coord
indexToCoord input =
    let
        index =
            normalizeIndex input
    in
    { x = index // 9
    , y = remainderBy 9 index
    }


coordToIndex : Coord -> Int
coordToIndex coord =
    let
        { x, y } =
            normalizeCoord coord
    in
    (x * 9) + y


normalizeIndex : Int -> Int
normalizeIndex index =
    if index < 0 then
        0

    else if index > 80 then
        80

    else
        index


normalizeCoord : Coord -> Coord
normalizeCoord coord =
    let
        y =
            if coord.y > 8 then
                8

            else if coord.y < 0 then
                0

            else
                coord.y

        x =
            if coord.x > 8 then
                8

            else if coord.x < 0 then
                0

            else
                coord.x
    in
    { x = x, y = y }


toRowsList : Grid -> List (List Cell)
toRowsList (Grid grid) =
    List.Extra.groupsOf 9 (Array.toList grid)



-- TYPE CONVERSIONS


fromArray : Array Cell -> Maybe Grid
fromArray input =
    if Array.length input == 81 then
        Just (Grid input)

    else
        Nothing


toArray : Grid -> Array Cell
toArray (Grid array) =
    array


fromList : List Cell -> Maybe Grid
fromList input =
    if List.length input == 81 then
        Just (Grid (Array.fromList input))

    else
        Nothing


toList : Grid -> List Cell
toList (Grid array) =
    Array.toList array


fromString : String -> Grid
fromString str =
    String.trim str
        |> String.left 81
        |> String.padLeft 81 '.'
        |> String.toList
        |> List.map cellFromChar
        |> Array.fromList
        |> Grid
