module Main exposing (main, parseRow, puzzleFromString)

import Array exposing (Array)
import Browser exposing (Document)
import Html exposing (..)
import Html.Attributes exposing (class, classList, cols, rows)
import Html.Events exposing (onClick, onInput)
import Tailwind.Utilities exposing (aspect_h_9)
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
    { currentPuzzle : Grid Value
    , selectedCell : Maybe Coord
    , puzzleInputMode : Bool
    }


initialModel : Model
initialModel =
    { currentPuzzle = puzzleFromString ""
    , selectedCell = Nothing
    , puzzleInputMode = True
    }



-- UPDATE


type Msg
    = ClickedCell { x : Int, y : Int }
    | UpdatedPuzzleInput String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedCell coord ->
            ( { model | selectedCell = Just coord }, Cmd.none )

        UpdatedPuzzleInput input ->
            ( { model | currentPuzzle = puzzleFromString input }, Cmd.none )



-- VIEW


view : Model -> Document Msg
view model =
    { title = "Sudoku"
    , body =
        [ viewPuzzle model.selectedCell model.currentPuzzle
        , viewPuzzleInput
        ]
    }


viewPuzzleInput : Html Msg
viewPuzzleInput =
    div [ class "puzzle-input" ]
        [ h3 [] [ text "Puzzle Input" ]
        , textarea [ rows 9, cols 9, onInput UpdatedPuzzleInput ] []
        ]


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
    List.map parseRow grid


parseRow : String -> List Value
parseRow input =
    input
        |> String.left 9
        |> String.padRight 9 '0'
        |> String.toList
        |> List.map Value.fromChar
