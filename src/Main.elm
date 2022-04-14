module Main exposing (main, parseRow, puzzleFromString)

import Array exposing (Array)
import Browser exposing (Document)
import Html exposing (..)
import Html.Attributes exposing (class, classList)
import Html.Events exposing (onClick)
import Value exposing (Value)



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


type alias Grid a =
    List (List a)


type alias Coord =
    { x : Int, y : Int }


gridMap : (a -> b) -> Grid a -> Grid b
gridMap fn grid =
    List.map (List.map fn) grid


type alias Model =
    { initialPuzzle : Grid Value
    , currentPuzzle : Grid Value
    , selectedCell : Maybe Coord
    }


initialModel : Model
initialModel =
    let
        puzzle =
            puzzleFromString puzzleStringA
    in
    { initialPuzzle = puzzle
    , currentPuzzle = puzzle
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
    { title = "Sudoku"
    , body = [ viewPuzzle model.selectedCell model.currentPuzzle ]
    }


viewPuzzle : Maybe Coord -> Grid Value -> Html Msg
viewPuzzle selectedCell puzzle =
    let
        selectedCoords =
            case selectedCell of
                Just coord ->
                    coord

                Nothing ->
                    { x = -1, y = -1 }

        viewValue : Int -> Int -> Value -> Html Msg
        viewValue x y value =
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
                    [ text <| Value.toString value ]
                ]

        viewRow y row =
            tr [] <| List.indexedMap (viewValue y) row
    in
    Html.table [ class "puzzle" ] <|
        List.indexedMap viewRow puzzle



-- MISC HELPERS


puzzleFromString : String -> Grid Value
puzzleFromString input =
    String.split "\n" input
        |> List.map parseRow


parseRow : String -> List Value
parseRow rowStr =
    rowStr
        |> String.trimLeft
        |> String.left 9
        |> String.toList
        |> List.map Value.fromChar


puzzleStringA : String
puzzleStringA =
    """.2...97..
..96...41
.7483..2.
.5...3.7.
.817.6...
2..5..136
56.9.8.1.
9....73.4
..2....5."""
