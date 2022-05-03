module GridBenchmark exposing (main)

import Benchmark exposing (..)
import Benchmark.Runner exposing (BenchmarkProgram, program)
import Sudoku.Grid as Grid exposing (..)


main : BenchmarkProgram
main =
    program suite


suite : Benchmark
suite =
    Debug.todo "benchmark suite"
