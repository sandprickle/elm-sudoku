module Sudoku.Cell exposing (Cell(..), fromChar, fromString, isFilled)

import Sudoku.Value as Value exposing (Value)


type Cell
    = Empty
    | Filled Value


fromChar : Char -> Cell
fromChar char =
    case Value.fromChar char of
        Just value ->
            Filled value

        Nothing ->
            Empty


fromString : String -> Cell
fromString str =
    case Value.fromString str of
        Just value ->
            Filled value

        Nothing ->
            Empty


isFilled : Cell -> Bool
isFilled cell =
    case cell of
        Filled _ ->
            True

        Empty ->
            False
