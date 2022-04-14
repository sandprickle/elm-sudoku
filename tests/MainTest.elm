module MainTest exposing (..)

import Array exposing (Array)
import Expect
import Main exposing (parseRow, puzzleFromString)
import Test exposing (Test, describe, test)
import Value exposing (Value)



-- TESTS


puzzleFromStringWorks : Test
puzzleFromStringWorks =
    test "multiline string produces expected grid" <|
        \_ ->
            let
                expectedResult =
                    [ [ 0, 2, 0, 0, 0, 9, 7, 0, 0 ]
                    , [ 0, 0, 9, 6, 0, 0, 0, 4, 1 ]
                    , [ 0, 7, 4, 8, 3, 0, 0, 2, 0 ]
                    , [ 0, 5, 0, 0, 0, 3, 0, 7, 0 ]
                    , [ 0, 8, 1, 7, 0, 6, 0, 0, 0 ]
                    , [ 2, 0, 0, 5, 0, 0, 1, 3, 6 ]
                    , [ 5, 6, 0, 9, 0, 8, 0, 1, 0 ]
                    , [ 9, 0, 0, 0, 0, 7, 3, 0, 4 ]
                    , [ 0, 0, 2, 0, 0, 0, 0, 5, 0 ]
                    ]

                inputString =
                    ".2...97..\n..96...41\n.7483..2.\n.5...3.7.\n.817.6...\n2..5..136\n56.9.8.1.\n9....73.4\n..2....5."

                transformRow : Array Value -> List Int
                transformRow row =
                    Array.map Value.toInt row
                        |> Array.toList
            in
            puzzleFromString inputString
                |> Array.map transformRow
                |> Array.toList
                |> Expect.equalLists expectedResult



-- HELPERS
