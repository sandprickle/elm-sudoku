module Sudoku.Cell exposing (Cell, fromChar, fromString, getValue, isFilled, toString)

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


isEmpty : Cell -> Bool
isEmpty cell =
    case cell of
        Filled _ ->
            False

        Empty ->
            True


toString : Cell -> String
toString cell =
    case cell of
        Empty ->
            ""

        Filled value ->
            Value.toString value


getValue : Cell -> Maybe Value
getValue cell =
    case cell of
        Filled value ->
            Just value

        Empty ->
            Nothing
