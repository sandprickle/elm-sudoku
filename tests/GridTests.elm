module GridTests exposing (..)

import Array
import Expect exposing (Expectation)
import Sudoku.Cell as Cell exposing (Cell(..))
import Sudoku.Grid as Grid
    exposing
        ( Coord
        , Grid
        , getBox
        , getCol
        , getRow
        , isLegal
        , toRows
        )
import Test exposing (..)



-- Utilities


testPuzzle : Grid
testPuzzle =
    Grid.fromString
        "....3.....2..1..4...7..9..6.1357..2.....8.5....6.....8........913..48...2649....."


testPuzzleIllegal : Grid
testPuzzleIllegal =
    Grid.fromString
        "...33.....2..1..4...7..9..6.1357..2.....8.5....6.....8........913..48...2649....."


testRows : List (List Cell)
testRows =
    List.map (List.map Cell.fromChar)
        [ [ '.', '.', '.', '.', '3', '.', '.', '.', '.' ]
        , [ '.', '2', '.', '.', '1', '.', '.', '4', '.' ]
        , [ '.', '.', '7', '.', '.', '9', '.', '.', '6' ]
        , [ '.', '1', '3', '5', '7', '.', '.', '2', '.' ]
        , [ '.', '.', '.', '.', '8', '.', '5', '.', '.' ]
        , [ '.', '.', '6', '.', '.', '.', '.', '.', '8' ]
        , [ '.', '.', '.', '.', '.', '.', '.', '.', '9' ]
        , [ '1', '3', '.', '.', '4', '8', '.', '.', '.' ]
        , [ '2', '6', '4', '9', '.', '.', '.', '.', '.' ]
        ]


testCols : List (List Cell)
testCols =
    List.map (List.map Cell.fromChar)
        [ [ '.', '.', '.', '.', '.', '.', '.', '1', '2' ]
        , [ '.', '2', '.', '1', '.', '.', '.', '3', '6' ]
        , [ '.', '.', '7', '3', '.', '6', '.', '.', '4' ]
        , [ '.', '.', '.', '5', '.', '.', '.', '.', '9' ]
        , [ '3', '1', '.', '7', '8', '.', '.', '4', '.' ]
        , [ '.', '.', '9', '.', '.', '.', '.', '8', '.' ]
        , [ '.', '.', '.', '.', '5', '.', '.', '.', '.' ]
        , [ '.', '4', '.', '2', '.', '.', '.', '.', '.' ]
        , [ '.', '.', '6', '.', '.', '8', '9', '.', '.' ]
        ]


testBoxes : List (List Cell)
testBoxes =
    List.map (List.map Cell.fromChar)
        [ [ '.', '.', '.', '.', '2', '.', '.', '.', '7' ]
        , [ '.', '3', '.', '.', '1', '.', '.', '.', '9' ]
        , [ '.', '.', '.', '.', '4', '.', '.', '.', '6' ]
        , [ '.', '1', '3', '.', '.', '.', '.', '.', '6' ]
        , [ '5', '7', '.', '.', '8', '.', '.', '.', '.' ]
        , [ '.', '2', '.', '5', '.', '.', '.', '.', '8' ]
        , [ '.', '.', '.', '1', '3', '.', '2', '6', '4' ]
        , [ '.', '.', '.', '.', '4', '8', '9', '.', '.' ]
        , [ '.', '.', '9', '.', '.', '.', '.', '.', '.' ]
        ]


toRowsTest : Test
toRowsTest =
    test "toRows works w/ valid input" <|
        \_ ->
            testPuzzle
                |> toRows
                |> Expect.equalLists testRows


getRowTest : Test
getRowTest =
    test "getRow works w/ valid input" <|
        \_ ->
            let
                allRows grid =
                    List.map (\n -> getRow n grid) (List.range 0 8)
            in
            Expect.equalLists testRows (allRows testPuzzle)


getColTest : Test
getColTest =
    test "getCol works w/ valid input" <|
        \_ ->
            let
                allCols grid =
                    List.map (\n -> getCol n grid) (List.range 0 8)
            in
            Expect.equalLists testCols (allCols testPuzzle)


getBoxTest : Test
getBoxTest =
    test "getBox works w/ valid input" <|
        \_ ->
            let
                allBoxes grid =
                    List.map (\n -> getBox n grid) (List.range 0 8)
            in
            Expect.equalLists testBoxes (allBoxes testPuzzle)


isLegalTest : Test
isLegalTest =
    describe "Grid.isLegal"
        [ test "true given valid puzzle" <|
            \_ ->
                Expect.equal True (isLegal testPuzzle)
        , test "false given invalid puzzle" <|
            \_ ->
                Expect.equal False (isLegal testPuzzleIllegal)
        ]
