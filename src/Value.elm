module Value exposing
    ( Value
    , fromChar
    , fromInt
    , fromString
    , matches
    , toInt
    , toString
    )


type Value
    = Value Int


matches : Value -> Value -> Bool
matches (Value a) (Value b) =
    a == b


toString : Value -> String
toString (Value int) =
    if int == 0 then
        ""

    else
        String.fromInt int


fromString : String -> Value
fromString str =
    if String.isEmpty str then
        Value 0

    else
        case String.left 1 str of
            "0" ->
                Value 0

            "1" ->
                Value 1

            "2" ->
                Value 2

            "3" ->
                Value 3

            "4" ->
                Value 4

            "5" ->
                Value 5

            "6" ->
                Value 6

            "7" ->
                Value 7

            "8" ->
                Value 8

            "9" ->
                Value 9

            _ ->
                Value 0


toInt : Value -> Int
toInt (Value int) =
    int


fromInt : Int -> Maybe Value
fromInt int =
    if int >= 0 && int <= 9 then
        Just (Value int)

    else
        Nothing


fromChar : Char -> Value
fromChar char =
    case char of
        '0' ->
            Value 0

        '1' ->
            Value 1

        '2' ->
            Value 2

        '3' ->
            Value 3

        '4' ->
            Value 4

        '5' ->
            Value 5

        '6' ->
            Value 6

        '7' ->
            Value 7

        '8' ->
            Value 8

        '9' ->
            Value 9

        _ ->
            Value 0
