module Main exposing (main)

import Browser exposing (Document)
import Html exposing (..)
import Html.Attributes exposing (class, classList)
import Html.Events exposing (onClick)
import Html.Lazy exposing (lazy, lazy2)
import Keyboard exposing (RawKey)
import Sudoku.CellValue as CellValue exposing (CellValue)
import Sudoku.Grid as Grid
    exposing
        ( Coord
        , Grid
        , fromString
        , getByCoord
        , setByCoord
        , toRows
        )



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
    , statusMsg : String
    , hintResult : String
    }


type PuzzleStatus
    = Valid
    | Invalid Coord


initialPuzzle : String
initialPuzzle =
    "....3.....2..1..4...7..9..6.1357..2.....8.5....6.....8........913..48...2649....."


initialModel : Model
initialModel =
    { currentPuzzle = Grid.fromString initialPuzzle
    , selectedCell = Nothing
    , statusMsg = "Not checked"
    , hintResult = ""
    }



-- UPDATE


type Msg
    = ClickedCell { x : Int, y : Int }
    | ClickedCheckPuzzle
    | KeyDown RawKey
    | RequestedHint Pattern


type Pattern
    = NakedSingle
    | HiddenSingle


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedCell coord ->
            ( { model | selectedCell = Just coord }, Cmd.none )

        ClickedCheckPuzzle ->
            let
                statusMsg =
                    if Grid.isLegal model.currentPuzzle then
                        "Puzzle is Legal"

                    else
                        "THAT'S ILLEGAL YOU DIMWIT!"
            in
            ( { model | statusMsg = statusMsg }, Cmd.none )

        KeyDown key ->
            handleKeyInput key model

        RequestedHint pattern ->
            ( { model | hintResult = getHint model.currentPuzzle pattern }
            , Cmd.none
            )


getHint : Grid -> Pattern -> String
getHint grid pattern =
    case pattern of
        NakedSingle ->
            String.concat
                [ "I count "
                , String.fromInt (Grid.countNakedSingles grid)
                , " naked singles in the current puzzle."
                ]

        HiddenSingle ->
            "I don't know how to check for hidden singles yet!"


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
                        Grid.pruneAll
                            (Grid.setByCoord
                                coord
                                model.currentPuzzle
                                (CellValue.fromString keyStr)
                            )
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
    if x == 0 then
        { x = 8, y = y }

    else
        { x = x - 1, y = y }


moveSelectionRight : Coord -> Coord
moveSelectionRight { x, y } =
    if x == 8 then
        { x = 0, y = y }

    else
        { x = x + 1, y = y }


moveSelectionUp : Coord -> Coord
moveSelectionUp { x, y } =
    if y == 0 then
        { x = x, y = 8 }

    else
        { x = x, y = y - 1 }


moveSelectionDown : Coord -> Coord
moveSelectionDown { x, y } =
    if y == 8 then
        { x = x, y = 0 }

    else
        { x = x, y = y + 1 }



-- VIEW


view : Model -> Document Msg
view model =
    { title = "Sudoku Trainer"
    , body =
        [ viewSidebar
            { statusMsg = model.statusMsg
            , selectedCoord = model.selectedCell
            , grid = model.currentPuzzle
            , hintResult = model.hintResult
            }
        , div [ class "main" ] [ viewPuzzle model.selectedCell model.currentPuzzle ]
        ]
    }


type alias SidebarConfig =
    { statusMsg : String
    , selectedCoord : Maybe Coord
    , hintResult : String
    , grid : Grid
    }


viewSidebar : SidebarConfig -> Html Msg
viewSidebar { statusMsg, selectedCoord, hintResult, grid } =
    let
        selectedText =
            case selectedCoord of
                Just coord ->
                    String.concat
                        [ "Row "
                        , String.fromInt (coord.y + 1)
                        , ", Col "
                        , String.fromInt (coord.x + 1)
                        ]

                Nothing ->
                    "None"

        possibleValues =
            case selectedCoord of
                Just coord ->
                    Grid.getByCoord coord grid
                        |> CellValue.getPossibleInts
                        |> List.map (\n -> String.fromInt n ++ " ")
                        |> String.concat

                Nothing ->
                    "N/A"
    in
    div [ class "sidebar" ]
        [ h1 [] [ text "Sudoku Trainer" ]
        , div [ class "cell-info" ]
            [ p [] [ text ("Selected: " ++ selectedText) ]
            , p [] [ text ("Possible Values: " ++ possibleValues) ]
            ]
        , div [ class "hints" ]
            [ h2 [] [ text "Hints" ]
            , div [ class "hint-btns" ]
                [ button
                    [ onClick (RequestedHint NakedSingle) ]
                    [ text "Naked Single" ]
                , button
                    [ onClick (RequestedHint HiddenSingle) ]
                    [ text "Hidden Single" ]
                ]
            , div [ class "hint-result" ] [ text hintResult ]
            ]
        ]


viewPuzzle : Maybe Coord -> Grid -> Html Msg
viewPuzzle selectedCoord puzzle =
    let
        viewRow : Int -> List CellValue -> Html Msg
        viewRow y row =
            tr [] <|
                List.indexedMap
                    (\x ->
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
    -> CellValue
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
            [ text (CellValue.toString cell) ]
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Keyboard.downs KeyDown
