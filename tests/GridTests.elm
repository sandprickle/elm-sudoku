module GridTests exposing (..)

import Array
import Expect exposing (Expectation)
import Sudoku.Grid as Grid
    exposing
        ( Cell(..)
        , Coord
        , Grid
        , cellFromChar
        , getBox
        , getCol
        , getRow
        )
import Test exposing (..)


testPuzzle : Grid
testPuzzle =
    Grid.fromString
        "....3.....2..1..4...7..9..6.1357..2.....8.5....6.....8........913..48...2649....."


getRowTest : Test
getRowTest =
    test "getRow works correctly w/ valid input" <|
        \_ ->
            testPuzzle
                |> getRow 6
                |> Expect.equal
                    (List.map cellFromChar
                        [ '.', '.', '.', '.', '.', '.', '.', '.', '9' ]
                    )


getColTest : Test
getColTest =
    test "getCol works w/ valid input" <|
        \_ ->
            testPuzzle
                |> getCol 6
                |> Expect.equal
                    (List.map cellFromChar
                        [ '.', '.', '.', '.', '5', '.', '.', '.', '.' ]
                    )


getBoxTest : Test
getBoxTest =
    test "getBox works w/ valid input" <|
        \_ ->
            testPuzzle
                |> getBox 5
                |> Expect.equal
                    (List.map cellFromChar
                        [ '.', '2', '.', '5', '.', '.', '.', '.', '8' ]
                    )
