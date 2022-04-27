module Sudoku.Grid exposing
    ( Coord
    , Grid
    , fromString
    , getByCoord
    , getByIndex
    , isLegal
    , setByCoord
    , toRowsList
    )

import Array exposing (Array)
import Array.Extra
import List.Extra
import Sudoku.Cell as Cell exposing (Cell(..), fromChar, fromString, isFilled)
import Sudoku.Value as Value exposing (Value)


type Grid
    = Grid (Array Cell)


getByCoord : Coord -> Grid -> Cell
getByCoord coord (Grid grid) =
    Array.get (coordToIndex coord) grid |> Maybe.withDefault Empty


setByCoord : Coord -> Grid -> Cell -> Grid
setByCoord coord (Grid grid) newCell =
    Array.set (coordToIndex coord) newCell grid |> Grid


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


toRows : Grid -> Array (Array Cell)
toRows grid =
    Debug.todo "toRows"


toCols : Grid -> Array (Array Cell)
toCols grid =
    Debug.todo "toCols"


toBoxes : Grid -> Array (Array Cell)
toBoxes grid =
    Debug.todo "toBoxes"



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
        |> List.map Cell.fromChar
        |> Array.fromList
        |> Grid



-- SUDOKU FUNCTIONS


isLegal : Grid -> Bool
isLegal grid =
    let
        rowsOk =
            Array.Extra.all noDuplicates (toRows grid)

        colsOk =
            Array.Extra.all noDuplicates (toCols grid)

        boxesOk =
            Array.Extra.all noDuplicates (toBoxes grid)
    in
    rowsOk && colsOk && boxesOk


noDuplicates : Array Cell -> Bool
noDuplicates cells =
    cells
        |> Array.filter Cell.isFilled
        |> Array.toList
        |> List.Extra.allDifferent
