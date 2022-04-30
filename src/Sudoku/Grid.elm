module Sudoku.Grid exposing
    ( Coord
    , Grid
    , fromString
    , getBox
    , getByCoord
    , getByIndex
    , getCol
    , getRow
    , setByCoord
    , toRowsList
    )

import Array exposing (Array)
import Array.Extra
import List.Extra
import Sudoku.Cell as Cell exposing (Cell(..), fromChar, fromString, isFilled)
import Sudoku.Value as Value exposing (Value)



-- Grid Type


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



-- Grid Conversions


toRows : Grid -> Array (Array Cell)
toRows grid =
    Debug.todo "toRows"


toCols : Grid -> Array (Array Cell)
toCols grid =
    Debug.todo "toCols"


toBoxes : Grid -> Array (Array Cell)
toBoxes grid =
    Debug.todo "toBoxes"


toRowsList : Grid -> List (List Cell)
toRowsList grid =
    List.map (\n -> getRow n grid) (List.range 0 8)


fromString : String -> Grid
fromString str =
    String.trim str
        |> String.left 81
        |> String.padLeft 81 '.'
        |> String.toList
        |> List.map Cell.fromChar
        |> Array.fromList
        |> Grid


getRow : Int -> Grid -> List Cell
getRow rowNum grid =
    let
        getCell i =
            getByCoord { x = rowNum, y = i } grid
    in
    List.map getCell (List.range 0 8)


getCol : Int -> Grid -> List Cell
getCol colNum grid =
    let
        getCell i =
            getByCoord { x = i, y = colNum } grid
    in
    List.map getCell (List.range 0 8)


getBox : Int -> Grid -> List Cell
getBox boxNum grid =
    let
        boxRow =
            boxNum // 3

        boxCol =
            modBy 3 boxNum

        xCoords =
            List.map (\n -> (3 * boxRow) + n) (List.range 0 2)

        yCoords =
            List.map (\n -> (3 * boxCol) + n) (List.range 0 2)

        getCell x y =
            getByCoord { x = x, y = y } grid

        getCellsInRow xCoord =
            List.map (getCell xCoord) yCoords
    in
    List.map getCellsInRow xCoords |> List.concat
