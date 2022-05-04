module Main exposing (main)

import Browser exposing (Document)
import Html exposing (..)
import Html.Attributes exposing (class, classList)
import Html.Events exposing (onClick)
import Html.Lazy exposing (lazy, lazy2)
import Keyboard exposing (RawKey)
import Sudoku.Cell as Cell exposing (Cell)
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
    | KeyDown RawKey


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

        KeyDown key ->
            handleKeyInput key model


handleKeyInput : RawKey -> Model -> ( Model, Cmd Msg )
handleKeyInput key model =
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

    else if List.member keyStr navKeys then
        case model.selectedCell of
            Nothing ->
                ( { model | selectedCell = Just { x = 4, y = 4 } }, Cmd.none )

            Just coord ->
                case keyStr of
                    "h" ->
                        ( { model | selectedCell = Just (moveSelectionLeft coord) }
                        , Cmd.none
                        )

                    "j" ->
                        ( { model | selectedCell = Just (moveSelectionDown coord) }
                        , Cmd.none
                        )

                    "k" ->
                        ( { model | selectedCell = Just (moveSelectionUp coord) }
                        , Cmd.none
                        )

                    "l" ->
                        ( { model | selectedCell = Just (moveSelectionRight coord) }
                        , Cmd.none
                        )

                    _ ->
                        ( model, Cmd.none )

    else
        ( model, Cmd.none )


valueKeys : List String
valueKeys =
    [ "0", "1", "2", "3", "4", "5", "6", "7", "8", "9" ]


navKeys : List String
navKeys =
    [ "h", "j", "k", "l" ]


moveSelectionLeft : Coord -> Coord
moveSelectionLeft { x, y } =
    if y == 0 then
        { x = x, y = 8 }

    else
        { x = x, y = y - 1 }


moveSelectionRight : Coord -> Coord
moveSelectionRight { x, y } =
    if y == 8 then
        { x = x, y = 0 }

    else
        { x = x, y = y + 1 }


moveSelectionUp : Coord -> Coord
moveSelectionUp { x, y } =
    if x == 0 then
        { x = 8, y = y }

    else
        { x = x - 1, y = y }


moveSelectionDown : Coord -> Coord
moveSelectionDown { x, y } =
    if x == 8 then
        { x = 0, y = y }

    else
        { x = x + 1, y = y }



-- VIEW


view : Model -> Document Msg
view model =
    { title = "Sudoku Trainer"
    , body =
        [ lazy viewSidebar model.puzzleStatus
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
viewPuzzle selectedCoord puzzle =
    let
        viewRow : Int -> List Cell -> Html Msg
        viewRow x row =
            tr [] <|
                List.indexedMap
                    (\y ->
                        lazy2 viewCell
                            { currentCoord = { x = x, y = y }
                            , selectedCoord = selectedCoord
                            }
                    )
                    row
    in
    Html.table [ class "puzzle" ] <|
        List.indexedMap viewRow (Grid.toRows puzzle)


viewCell :
    { currentCoord : Coord
    , selectedCoord : Maybe Coord
    }
    -> Cell
    -> Html Msg
viewCell { currentCoord, selectedCoord } cell =
    let
        { x, y } =
            currentCoord

        selected =
            case selectedCoord of
                Just coord ->
                    x == coord.x && y == coord.y

                Nothing ->
                    False
    in
    td []
        [ div
            [ classList
                [ ( "value selected", selected )
                , ( "value", not selected )
                ]
            , onClick (ClickedCell (Coord x y))
            ]
            [ text (Cell.toString cell) ]
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Keyboard.downs KeyDown
