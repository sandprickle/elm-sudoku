module Sudoku.CellValue exposing
    ( CellValue
    , empty
    , fromChar
    , fromPossibleNumbers
    , fromString
    , getNumber
    , getPossible
    , getPossibleInts
    , isEmpty
    , isFilled
    , toString
    )

import Sudoku.Number as Number exposing (Number)


type CellValue
    = Empty (List Number)
    | Filled Number


allPossibilities : List Number
allPossibilities =
    [ Number.one
    , Number.two
    , Number.three
    , Number.four
    , Number.five
    , Number.six
    , Number.seven
    , Number.eight
    , Number.nine
    ]


fromChar : Char -> CellValue
fromChar char =
    case Number.fromChar char of
        Just value ->
            Filled value

        Nothing ->
            Empty allPossibilities


fromString : String -> CellValue
fromString str =
    case Number.fromString str of
        Just value ->
            Filled value

        Nothing ->
            Empty allPossibilities


fromPossibleNumbers : List Number -> CellValue
fromPossibleNumbers values =
    Empty values


isFilled : CellValue -> Bool
isFilled cell =
    case cell of
        Filled _ ->
            True

        Empty _ ->
            False


isEmpty : CellValue -> Bool
isEmpty cell =
    case cell of
        Filled _ ->
            False

        Empty _ ->
            True


toString : CellValue -> String
toString cell =
    case cell of
        Empty _ ->
            ""

        Filled value ->
            Number.toString value


getNumber : CellValue -> Maybe Number
getNumber cell =
    case cell of
        Filled value ->
            Just value

        Empty _ ->
            Nothing


getPossible : CellValue -> Maybe (List Number)
getPossible cell =
    case cell of
        Empty values ->
            Just values

        Filled _ ->
            Nothing


getPossibleInts : CellValue -> List Int
getPossibleInts cell =
    case cell of
        Empty values ->
            List.map Number.toInt values

        Filled value ->
            [ Number.toInt value ]


empty : CellValue
empty =
    Empty allPossibilities
