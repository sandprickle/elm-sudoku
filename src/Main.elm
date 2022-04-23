module Main exposing (main)

import Browser exposing (Document)
import Html exposing (..)
import Html.Attributes exposing (class, classList)
import Html.Events exposing (onClick)
import Sudoku.Value as Value exposing (Value)



-- MAIN


main : Program () Model Msg
main =
    Browser.document
        { init = \_ -> ( initialModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }



-- MODEL


type alias Grid =
    List (List Cell)


type Cell
    = Filled Value
    | Empty


type alias Coord =
    { x : Int, y : Int }


type alias Model =
    { currentPuzzle : Grid
    , selectedCell : Maybe Coord
    }


initialPuzzle : String
initialPuzzle =
    ".7..18.94\n.4.....67\n2......1.\n....59..6\n....4....\n3..18....\n.9......1\n71.....8.\n68.53..7."


initialModel : Model
initialModel =
    { currentPuzzle = puzzleFromString initialPuzzle
    , selectedCell = Nothing
    }



-- UPDATE


type Msg
    = ClickedCell { x : Int, y : Int }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedCell coord ->
            ( { model | selectedCell = Just coord }, Cmd.none )



-- VIEW


view : Model -> Document Msg
view model =
    { title = "Sudoku Trainer"
    , body =
        [ viewSidebar
        , div [ class "main" ] [ viewPuzzle model.selectedCell model.currentPuzzle ]
        ]
    }


viewSidebar : Html Msg
viewSidebar =
    div [ class "sidebar" ]
        [ h1 [] [ text "Sudoku Trainer" ]
        ]


viewPuzzle : Maybe Coord -> Grid -> Html Msg
viewPuzzle selectedCell puzzle =
    let
        selectedCoords =
            case selectedCell of
                Just coord ->
                    coord

                Nothing ->
                    { x = -1, y = -1 }

        viewCell : Int -> Int -> Cell -> Html Msg
        viewCell x y cell =
            let
                selected =
                    x == selectedCoords.x && y == selectedCoords.y
            in
            td []
                [ div
                    [ classList
                        [ ( "value selected", selected )
                        , ( "value", not selected )
                        ]
                    , onClick (ClickedCell (Coord x y))
                    ]
                    [ case cell of
                        Filled value ->
                            text (Value.toString value)

                        Empty ->
                            text ""
                    ]
                ]

        viewRow y row =
            tr [] <| List.indexedMap (viewCell y) row
    in
    Html.table [ class "puzzle" ] <|
        List.indexedMap viewRow puzzle



-- MISC HELPERS


puzzleFromString : String -> Grid
puzzleFromString input =
    let
        blankRow =
            "........."

        rows =
            input |> String.split "\n" |> List.take 9

        numRows =
            List.length rows

        grid =
            if numRows < 9 then
                List.append rows
                    (List.repeat (9 - numRows) blankRow)

            else
                rows
    in
    List.map rowFromString grid


rowFromString : String -> List Cell
rowFromString input =
    input
        |> String.left 9
        |> String.padRight 9 '0'
        |> String.toList
        |> List.map cellFromChar


cellFromChar : Char -> Cell
cellFromChar char =
    case Value.fromChar char of
        Just value ->
            Filled value

        Nothing ->
            Empty
