module Sudoku.Value exposing
    ( Value
    , eight
    , five
    , four
    , fromChar
    , fromInt
    , fromString
    , nine
    , one
    , seven
    , six
    , three
    , toString
    , two
    )


type Value
    = Value Int


toString : Value -> String
toString (Value int) =
    String.fromInt int


fromString : String -> Maybe Value
fromString str =
    case str of
        "1" ->
            Just (Value 1)

        "2" ->
            Just (Value 2)

        "3" ->
            Just (Value 3)

        "4" ->
            Just (Value 4)

        "5" ->
            Just (Value 5)

        "6" ->
            Just (Value 6)

        "7" ->
            Just (Value 7)

        "8" ->
            Just (Value 8)

        "9" ->
            Just (Value 9)

        _ ->
            Nothing


fromInt : Int -> Maybe Value
fromInt int =
    if int >= 1 && int <= 9 then
        Just (Value int)

    else
        Nothing


fromChar : Char -> Maybe Value
fromChar char =
    case char of
        '1' ->
            Just (Value 1)

        '2' ->
            Just (Value 2)

        '3' ->
            Just (Value 3)

        '4' ->
            Just (Value 4)

        '5' ->
            Just (Value 5)

        '6' ->
            Just (Value 6)

        '7' ->
            Just (Value 7)

        '8' ->
            Just (Value 8)

        '9' ->
            Just (Value 9)

        _ ->
            Nothing



-- Hardcoded Values


one : Value
one =
    Value 1


two : Value
two =
    Value 2


three : Value
three =
    Value 3


four : Value
four =
    Value 4


five : Value
five =
    Value 5


six : Value
six =
    Value 6


seven : Value
seven =
    Value 7


eight : Value
eight =
    Value 8


nine : Value
nine =
    Value 9
