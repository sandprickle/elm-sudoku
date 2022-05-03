module GridBenchmark exposing (main)

import Benchmark exposing (..)
import Benchmark.Runner exposing (BenchmarkProgram, program)
import Sudoku.Grid as Grid exposing (..)


main : BenchmarkProgram
main =
    program suite


suite : Benchmark
suite =
    let
        puzzleString =
            "....3.....2..1..4...7..9..6.1357..2.....8.5....6.....8........913..48...2649....."

        grid =
            Grid.fromString puzzleString
    in
    describe "Grid Functions" <|
        [ benchmark "fromString" <|
            \_ ->
                Grid.fromString puzzleString
        , benchmark "isLegal" <|
            \_ ->
                Grid.isLegal grid
        ]
