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
    | Blank


matches : Value -> Value -> Bool
matches val1 val2 =
    case ( val1, val2 ) of
        ( Value a, Value b ) ->
            a == b

        _ ->
            False


toString : Value -> String
toString val =
    case val of
        Value int ->
            String.fromInt int

        Blank ->
            ""


fromString : String -> Value
fromString str =
    if String.isEmpty str then
        Blank

    else
        case String.left 1 str of
            "0" ->
                Blank

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
                Blank


toInt : Value -> Int
toInt val =
    case val of
        Value int ->
            int

        Blank ->
            0


fromInt : Int -> Maybe Value
fromInt int =
    if int == 0 then
        Just Blank

    else if int >= 1 && int <= 9 then
        Just (Value int)

    else
        Nothing


fromChar : Char -> Value
fromChar char =
    case char of
        '0' ->
            Blank

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
            Blank
