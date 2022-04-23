module Main exposing (main)

import Browser exposing (Document)
import Html exposing (..)
import Html.Attributes exposing (class, classList)
import Html.Events exposing (onClick)
import Keyboard exposing (RawKey)
import Sudoku.Grid as Grid
    exposing
        ( Cell(..)
        , Coord
        , Grid
        , fromString
        , getByCoord
        , setByCoord
        , toRowsList
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
    }


initialPuzzle : String
initialPuzzle =
    ".7..18.94.4.....672......1.....59..6....4....3..18.....9......171.....8.68.53..7."


initialModel : Model
initialModel =
    { currentPuzzle = Grid.fromString initialPuzzle
    , selectedCell = Nothing
    }



-- UPDATE


type Msg
    = ClickedCell { x : Int, y : Int }
    | KeyUp RawKey


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedCell coord ->
            ( { model | selectedCell = Just coord }, Cmd.none )

        KeyUp key ->
            let
                keyChar : Char
                keyChar =
                    case Keyboard.characterKeyOriginal key of
                        Just (Keyboard.Character str) ->
                            String.uncons str
                                |> Maybe.withDefault ( '.', "" )
                                |> Tuple.first

                        _ ->
                            '.'
            in
            if List.member keyChar valueKeys then
                case model.selectedCell of
                    Just coord ->
                        ( { model
                            | currentPuzzle =
                                Grid.setByCoord
                                    coord
                                    model.currentPuzzle
                                    keyChar
                          }
                        , Cmd.none
                        )

                    Nothing ->
                        ( model, Cmd.none )

            else
                ( model, Cmd.none )


valueKeys : List Char
valueKeys =
    [ '1', '2', '3', '4', '5', '6', '7', '8', '9' ]



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
        List.indexedMap viewRow (Grid.toRowsList puzzle)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Keyboard.ups KeyUp
