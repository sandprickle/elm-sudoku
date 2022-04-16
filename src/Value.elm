module Value exposing
    ( Value
    , fromChar
    , fromInt
    , fromString
    , toInt
    , toString
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


toInt : Value -> Int
toInt (Value int) =
    int


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
