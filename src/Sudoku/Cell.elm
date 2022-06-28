module Sudoku.Cell exposing
    ( Cell
    , Highlight(..)
    , getHighlight
    , getPrimaryNotes
    , getSecondaryNotes
    , getValue
    )

import Sudoku.CellValue as CellValue exposing (CellValue)
import Sudoku.Number as Number exposing (Number)


type Cell
    = Cell
        { value : CellValue
        , primaryNotes : List Number
        , secondaryNotes : List Number
        , highlight : Highlight
        }


type Highlight
    = Error
    | None


getValue : Cell -> CellValue
getValue (Cell cell) =
    cell.value


getPrimaryNotes : Cell -> List Number
getPrimaryNotes (Cell cell) =
    cell.primaryNotes


getSecondaryNotes : Cell -> List Number
getSecondaryNotes (Cell cell) =
    cell.secondaryNotes


getHighlight : Cell -> Highlight
getHighlight (Cell cell) =
    cell.highlight


mapValue : CellValue -> Cell -> Cell
mapValue value (Cell cell) =
    Cell { cell | value = value }


mapPrimaryNotes : List Number -> Cell -> Cell
mapPrimaryNotes notes (Cell cell) =
    Cell { cell | primaryNotes = notes }


mapSecondaryNotes : List Number -> Cell -> Cell
mapSecondaryNotes notes (Cell cell) =
    Cell { cell | secondaryNotes = notes }


mapHighlight : Highlight -> Cell -> Cell
mapHighlight highlight (Cell cell) =
    Cell { cell | highlight = highlight }
