module Main exposing (main)

import Browser exposing (Document)
import Html exposing (..)
import Html.Attributes exposing (class, classList)
import Html.Events exposing (onClick)
import Keyboard exposing (RawKey)
import Sudoku.Cell as Cell exposing (Cell(..))
import Sudoku.Grid as Grid
    exposing
        ( Coord
        , Grid
        , fromString
        , getByCoord
        , setByCoord
        , toRows
        )
import Sudoku.Value as Value exposing (Value)



-- MAIN


main : Program () Model Msg
main =
    Browser.document
        { init = \_ -> ( initialModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { currentPuzzle : Grid
    , selectedCell : Maybe Coord
    , puzzleStatus : String
    }


initialPuzzle : String
initialPuzzle =
    "....3.....2..1..4...7..9..6.1357..2.....8.5....6.....8........913..48...2649....."


initialModel : Model
initialModel =
    { currentPuzzle = Grid.fromString initialPuzzle
    , selectedCell = Nothing
    , puzzleStatus = "Not checked"
    }



-- UPDATE


type Msg
    = ClickedCell { x : Int, y : Int }
    | ClickedCheckPuzzle
    | KeyUp RawKey


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedCell coord ->
            ( { model | selectedCell = Just coord }, Cmd.none )

        ClickedCheckPuzzle ->
            let
                puzzleStatus =
                    if Grid.isLegal model.currentPuzzle then
                        "Puzzle is Legal"

                    else
                        "THAT'S ILLEGAL YOU DIMWIT!"
            in
            ( { model | puzzleStatus = puzzleStatus }, Cmd.none )

        KeyUp key ->
            let
                keyStr : String
                keyStr =
                    case Keyboard.characterKeyOriginal key of
                        Just (Keyboard.Character str) ->
                            str

                        _ ->
                            ""
            in
            if List.member keyStr valueKeys then
                case model.selectedCell of
                    Just coord ->
                        ( { model
                            | currentPuzzle =
                                Grid.setByCoord
                                    coord
                                    model.currentPuzzle
                                    (Cell.fromString keyStr)
                          }
                        , Cmd.none
                        )

                    Nothing ->
                        ( model, Cmd.none )

            else
                ( model, Cmd.none )


valueKeys : List String
valueKeys =
    [ "0", "1", "2", "3", "4", "5", "6", "7", "8", "9" ]



-- VIEW


view : Model -> Document Msg
view model =
    { title = "Sudoku Trainer"
    , body =
        [ viewSidebar model.puzzleStatus
        , div [ class "main" ] [ viewPuzzle model.selectedCell model.currentPuzzle ]
        ]
    }


viewSidebar : String -> Html Msg
viewSidebar puzzleStatus =
    div [ class "sidebar" ]
        [ h1 [] [ text "Sudoku Trainer" ]
        , button [ onClick ClickedCheckPuzzle ] [ text "Check Puzzle" ]
        , p [ class "puzzle-status" ] [ text puzzleStatus ]
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
        List.indexedMap viewRow (Grid.toRows puzzle)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Keyboard.ups KeyUp
