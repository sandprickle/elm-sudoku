module Sudoku.Cell exposing
    ( Cell
    , empty
    , fromChar
    , fromPossibleValues
    , fromString
    , getPossible
    , getValue
    , isEmpty
    , isFilled
    , toString
    )

import Sudoku.Value as Value exposing (Value)


type Cell
    = Empty (List Value)
    | Filled Value


allPossibilities : List Value
allPossibilities =
    [ Value.one
    , Value.two
    , Value.three
    , Value.four
    , Value.five
    , Value.six
    , Value.seven
    , Value.eight
    , Value.nine
    ]


fromChar : Char -> Cell
fromChar char =
    case Value.fromChar char of
        Just value ->
            Filled value

        Nothing ->
            Empty allPossibilities


fromString : String -> Cell
fromString str =
    case Value.fromString str of
        Just value ->
            Filled value

        Nothing ->
            Empty allPossibilities


fromPossibleValues : List Value -> Cell
fromPossibleValues values =
    Empty values


isFilled : Cell -> Bool
isFilled cell =
    case cell of
        Filled _ ->
            True

        Empty _ ->
            False


isEmpty : Cell -> Bool
isEmpty cell =
    case cell of
        Filled _ ->
            False

        Empty _ ->
            True


toString : Cell -> String
toString cell =
    case cell of
        Empty _ ->
            ""

        Filled value ->
            Value.toString value


getValue : Cell -> Maybe Value
getValue cell =
    case cell of
        Filled value ->
            Just value

        Empty _ ->
            Nothing


getPossible : Cell -> Maybe (List Value)
getPossible cell =
    case cell of
        Empty values ->
            Just values

        Filled _ ->
            Nothing


empty : Cell
empty =
    Empty allPossibilities
