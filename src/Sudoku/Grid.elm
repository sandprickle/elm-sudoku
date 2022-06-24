module Sudoku.Grid exposing
    ( Coord
    , Grid
    , boxCoords
    , colCoords
    , countNakedSingles
    , fromString
    , getBox
    , getByCoord
    , getByIndex
    , getCol
    , getRow
    , isLegal
    , pruneAll
    , pruneBoxes
    , pruneCellValues
    , pruneCols
    , pruneRows
    , removeFixed
    , rowCoords
    , setByCoord
    , toBoxes
    , toCols
    , toRows
    )

import Array exposing (Array)
import Html exposing (a)
import List.Extra exposing (allDifferent)
import Sudoku.CellValue as CellValue
    exposing
        ( CellValue
        , fromChar
        , fromString
        , isFilled
        )
import Sudoku.Number as Number exposing (Number)


type alias Coord =
    { x : Int, y : Int }


type Grid
    = Grid (Array CellValue)


getByCoord : Coord -> Grid -> CellValue
getByCoord coord (Grid grid) =
    Array.get (coordToIndex coord) grid |> Maybe.withDefault (CellValue.fromString "")


getByIndex : Int -> Grid -> CellValue
getByIndex index (Grid grid) =
    Array.get (normalizeIndex index) grid
        |> Maybe.withDefault (CellValue.fromString "")


setByCoord : Coord -> Grid -> CellValue -> Grid
setByCoord coord (Grid grid) newCellValue =
    Array.set (coordToIndex coord) newCellValue grid |> Grid



-- Conversions


fromString : String -> Grid
fromString str =
    String.trim str
        |> String.left 81
        |> String.padLeft 81 '.'
        |> String.toList
        |> List.map CellValue.fromChar
        |> Array.fromList
        |> Grid
        |> pruneAll



-- Row/Col/Box Helpers
-- To Lists of groupings


toRows : Grid -> List (List CellValue)
toRows grid =
    List.map (\n -> getRow n grid) (List.range 0 8)


toCols : Grid -> List (List CellValue)
toCols grid =
    List.map (\n -> getCol n grid) (List.range 0 8)


toBoxes : Grid -> List (List CellValue)
toBoxes grid =
    List.map (\n -> getBox n grid) (List.range 0 8)



-- individual groupings


getRow : Int -> Grid -> List CellValue
getRow rowNum grid =
    List.map (\coord -> getByCoord coord grid) (rowCoords rowNum)


getCol : Int -> Grid -> List CellValue
getCol colNum grid =
    List.map (\coord -> getByCoord coord grid) (colCoords colNum)


getBox : Int -> Grid -> List CellValue
getBox boxNum grid =
    List.map (\coord -> getByCoord coord grid) (boxCoords boxNum)



-- Lists of coordinates


rowCoords : Int -> List Coord
rowCoords rowNum =
    let
        getCoord i =
            { x = i, y = rowNum }
    in
    List.map getCoord (List.range 0 8)


colCoords : Int -> List Coord
colCoords colNum =
    let
        getCoord i =
            { x = colNum, y = i }
    in
    List.map getCoord (List.range 0 8)


boxCoords : Int -> List Coord
boxCoords boxNum =
    let
        boxRow =
            boxNum // 3

        boxCol =
            modBy 3 boxNum

        yCoords =
            List.map (\n -> (3 * boxRow) + n) (List.range 0 2)

        xCoords =
            List.map (\n -> (3 * boxCol) + n) (List.range 0 2)

        getCoord y x =
            { x = x, y = y }

        coordsInRow yCoord =
            List.map (getCoord yCoord) xCoords
    in
    List.map coordsInRow yCoords |> List.concat



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

        checkGroup : List CellValue -> Bool
        checkGroup group =
            group
                |> List.filter CellValue.isFilled
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
        fn : ( Coord, CellValue ) -> Grid -> Grid
        fn ( coord, cell ) grid_ =
            setByCoord coord grid_ cell
    in
    coords
        |> List.map (\coord -> getByCoord coord grid)
        |> pruneCellValues
        |> List.map2 (\coord cell -> ( coord, cell )) coords
        |> List.foldl fn grid


pruneCellValues : List CellValue -> List CellValue
pruneCellValues cells =
    let
        fixedNumbers =
            List.filterMap CellValue.getNumber cells

        pruneCellValue : CellValue -> CellValue
        pruneCellValue cell =
            case CellValue.getPossible cell of
                Just values ->
                    values
                        |> removeFixed fixedNumbers
                        |> CellValue.fromPossibleNumbers

                Nothing ->
                    cell
    in
    List.map pruneCellValue cells



-- Hints


countNakedSingles : Grid -> Int
countNakedSingles (Grid grid) =
    let
        nakedSingle cell =
            case CellValue.getPossible cell of
                Just possible ->
                    List.length possible == 1

                Nothing ->
                    False
    in
    grid
        |> Array.filter nakedSingle
        |> Array.length



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
    -- this might be wrong!
    { x = index // 9
    , y = remainderBy 9 index
    }


coordToIndex : Coord -> Int
coordToIndex coord =
    let
        { x, y } =
            normalizeCoord coord
    in
    (y * 9) + x


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
