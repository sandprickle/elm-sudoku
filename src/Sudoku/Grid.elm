module Sudoku.Grid exposing
    ( Coord
    , Grid
    , fromString
    , getBox
    , getByCoord
    , getByIndex
    , getCol
    , getRow
    , isLegal
    , pruneAll
    , pruneBoxes
    , pruneCells
    , pruneCols
    , pruneRows
    , removeFixed
    , setByCoord
    , toRows
    )

import Array exposing (Array)
import Html exposing (a)
import List.Extra exposing (allDifferent)
import Sudoku.Cell as Cell exposing (Cell, fromChar, fromString, isFilled)
import Sudoku.Value as Value exposing (Value)


type alias Coord =
    { x : Int, y : Int }


type Grid
    = Grid (Array Cell)


getByCoord : Coord -> Grid -> Cell
getByCoord coord (Grid grid) =
    Array.get (coordToIndex coord) grid |> Maybe.withDefault (Cell.fromString "")


getByIndex : Int -> Grid -> Cell
getByIndex index (Grid grid) =
    Array.get (normalizeIndex index) grid
        |> Maybe.withDefault (Cell.fromString "")


setByCoord : Coord -> Grid -> Cell -> Grid
setByCoord coord (Grid grid) newCell =
    Array.set (coordToIndex coord) newCell grid |> Grid



-- Conversions


fromString : String -> Grid
fromString str =
    String.trim str
        |> String.left 81
        |> String.padLeft 81 '.'
        |> String.toList
        |> List.map Cell.fromChar
        |> Array.fromList
        |> Grid



-- Row/Col/Box Helpers
-- To Lists of groupings


toRows : Grid -> List (List Cell)
toRows grid =
    List.map (\n -> getRow n grid) (List.range 0 8)


toCols : Grid -> List (List Cell)
toCols grid =
    List.map (\n -> getCol n grid) (List.range 0 8)


toBoxes : Grid -> List (List Cell)
toBoxes grid =
    List.map (\n -> getBox n grid) (List.range 0 8)



-- individual groupings


getRow : Int -> Grid -> List Cell
getRow rowNum grid =
    List.map (\coord -> getByCoord coord grid) (rowCoords rowNum)


getCol : Int -> Grid -> List Cell
getCol colNum grid =
    List.map (\coord -> getByCoord coord grid) (colCoords colNum)


getBox : Int -> Grid -> List Cell
getBox boxNum grid =
    List.map (\coord -> getByCoord coord grid) (boxCoords boxNum)



-- Lists of coordinates


rowCoords : Int -> List Coord
rowCoords rowNum =
    let
        getCoord i =
            { x = rowNum, y = i }
    in
    List.map getCoord (List.range 0 8)


colCoords : Int -> List Coord
colCoords colNum =
    let
        getCoord i =
            { x = i, y = colNum }
    in
    List.map getCoord (List.range 0 8)


boxCoords : Int -> List Coord
boxCoords boxNum =
    let
        boxRow =
            boxNum // 3

        boxCol =
            modBy 3 boxNum

        xCoords =
            List.map (\n -> (3 * boxRow) + n) (List.range 0 2)

        yCoords =
            List.map (\n -> (3 * boxCol) + n) (List.range 0 2)

        getCoord x y =
            { x = x, y = y }

        coordsInRow xCoord =
            List.map (getCoord xCoord) yCoords
    in
    List.map coordsInRow xCoords |> List.concat



-- Constraints


isLegal : Grid -> Bool
isLegal grid =
    let
        rowsOk =
            grid
                |> toRows
                |> List.map checkGroup
                |> List.member False
                |> not

        colsOk =
            grid
                |> toCols
                |> List.map checkGroup
                |> List.member False
                |> not

        boxesOk =
            grid
                |> toBoxes
                |> List.map checkGroup
                |> List.member False
                |> not

        checkGroup : List Cell -> Bool
        checkGroup group =
            group
                |> List.filter Cell.isFilled
                |> allDifferent
    in
    rowsOk && colsOk && boxesOk


pruneAll : Grid -> Grid
pruneAll grid =
    let
        newGrid =
            grid
                |> pruneRows
                |> pruneCols
                |> pruneBoxes
    in
    if grid == newGrid then
        grid

    else
        pruneAll newGrid


pruneRows : Grid -> Grid
pruneRows grid =
    let
        rows =
            List.map rowCoords (List.range 0 8)
    in
    List.foldl pruneReducer grid rows


pruneCols : Grid -> Grid
pruneCols grid =
    let
        cols =
            List.map colCoords (List.range 0 8)
    in
    List.foldl pruneReducer grid cols


pruneBoxes : Grid -> Grid
pruneBoxes grid =
    let
        boxes =
            List.map boxCoords (List.range 0 8)
    in
    List.foldl pruneReducer grid boxes


pruneReducer : List Coord -> Grid -> Grid
pruneReducer coords grid =
    let
        fn : ( Coord, Cell ) -> Grid -> Grid
        fn ( coord, cell ) grid_ =
            setByCoord coord grid_ cell
    in
    coords
        |> List.map (\coord -> getByCoord coord grid)
        |> pruneCells
        |> List.map2 (\coord cell -> ( coord, cell )) coords
        |> List.foldl fn grid


pruneCells : List Cell -> List Cell
pruneCells cells =
    let
        fixedValues =
            List.filterMap Cell.getValue cells

        pruneCell : Cell -> Cell
        pruneCell cell =
            case Cell.getPossible cell of
                Just values ->
                    values
                        |> removeFixed fixedValues
                        |> Cell.fromPossibleValues

                Nothing ->
                    cell
    in
    List.map pruneCell cells



-- Internal Helpers


removeFixed : List a -> List a -> List a
removeFixed fixedVals possible =
    List.foldl List.Extra.remove possible fixedVals


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
