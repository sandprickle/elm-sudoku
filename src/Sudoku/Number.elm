module Sudoku.Number exposing
    ( Number
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
    , toInt
    , toString
    , two
    )


type Number
    = Number Int


toInt : Number -> Int
toInt (Number int) =
    int


toString : Number -> String
toString (Number int) =
    String.fromInt int


fromString : String -> Maybe Number
fromString str =
    case str of
        "1" ->
            Just (Number 1)

        "2" ->
            Just (Number 2)

        "3" ->
            Just (Number 3)

        "4" ->
            Just (Number 4)

        "5" ->
            Just (Number 5)

        "6" ->
            Just (Number 6)

        "7" ->
            Just (Number 7)

        "8" ->
            Just (Number 8)

        "9" ->
            Just (Number 9)

        _ ->
            Nothing


fromInt : Int -> Maybe Number
fromInt int =
    if int >= 1 && int <= 9 then
        Just (Number int)

    else
        Nothing


fromChar : Char -> Maybe Number
fromChar char =
    case char of
        '1' ->
            Just (Number 1)

        '2' ->
            Just (Number 2)

        '3' ->
            Just (Number 3)

        '4' ->
            Just (Number 4)

        '5' ->
            Just (Number 5)

        '6' ->
            Just (Number 6)

        '7' ->
            Just (Number 7)

        '8' ->
            Just (Number 8)

        '9' ->
            Just (Number 9)

        _ ->
            Nothing



-- Hardcoded Numbers


one : Number
one =
    Number 1


two : Number
two =
    Number 2


three : Number
three =
    Number 3


four : Number
four =
    Number 4


five : Number
five =
    Number 5


six : Number
six =
    Number 6


seven : Number
seven =
    Number 7


eight : Number
eight =
    Number 8


nine : Number
nine =
    Number 9
