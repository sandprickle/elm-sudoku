module Main exposing (..)

import Browser
import Html exposing (Html, div, span, table, tbody, td, text, tr)
import Html.Attributes exposing (..)



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- Model


type alias Model =
    { solution : Maybe Puzzle
    , currentPuzzle : Puzzle
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { solution = Nothing
      , currentPuzzle =
            [ [ Blank, Four, Blank, Two, Seven, Blank, Blank, Blank, Blank ]
            , [ Six, Blank, Blank, Blank, Three, Blank, Blank, Blank, Blank ]
            , [ Blank, Blank, Blank, Five, Six, Blank, Blank, Seven, Two ]
            , [ Blank, Blank, Blank, Blank, Five, Blank, One, Blank, Seven ]
            , [ Blank, Blank, Blank, Three, Blank, Blank, Blank, Blank, Blank ]
            , [ Nine, Blank, Five, Blank, Four, Blank, Blank, Blank, Blank ]
            , [ Blank, Blank, Blank, Four, Blank, Blank, Blank, Nine, Blank ]
            , [ Blank, Seven, Blank, Blank, One, Blank, Three, Blank, Blank ]
            , [ Blank, Blank, Eight, Blank, Blank, Blank, Two, One, Four ]
            ]
      }
    , Cmd.none
    )



-- Update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- View


view : Model -> Html Msg
view model =
    div []
        [ viewPuzzle model.currentPuzzle ]



-- Helpers & Types


viewValue : Value -> Html Msg
viewValue value =
    td [ class "border-2 border-gray-500 w-12 h-12" ]
        [ case value of
            Blank ->
                text ""

            _ ->
                div [ class "w-full h-full flex items-center justify-center text-xl" ]
                    [ text (valueToString value)
                    ]
        ]


viewRow : List Value -> Html Msg
viewRow row =
    tr [] (List.map viewValue row)


viewPuzzle : Puzzle -> Html Msg
viewPuzzle puzzle =
    table
        [ class "" ]
        [ tbody
            []
            (List.map viewRow puzzle)
        ]


valueToString : Value -> String
valueToString value =
    case value of
        Blank ->
            ""

        One ->
            "1"

        Two ->
            "2"

        Three ->
            "3"

        Four ->
            "4"

        Five ->
            "5"

        Six ->
            "6"

        Seven ->
            "7"

        Eight ->
            "8"

        Nine ->
            "9"


type Msg
    = Change String


type Value
    = Blank
    | One
    | Two
    | Three
    | Four
    | Five
    | Six
    | Seven
    | Eight
    | Nine


type alias Cell =
    { value : Value
    , possible : List Value
    , snyder : List Value
    }


type alias Grid t =
    List (List t)


type alias Puzzle =
    Grid Value
