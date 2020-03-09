module Utc exposing (fromTime, toTime, decoder, encode)

{-| Convert between UTC date strings and POSIX times.

@docs fromTime, toTime, decoder, encode

-}

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Parser exposing ((|.), (|=), Parser, andThen, end, oneOf, spaces, succeed, symbol)
import Time exposing (Month(..), Weekday(..), utc)


{-| Decode a UTC date string to a `Time.Posix` value using [`toTime`](#toTime).
-}
decoder : Decoder Time.Posix
decoder =
    Decode.string
        |> Decode.andThen
            (\str ->
                case toTime str of
                    Err deadEnds ->
                        Decode.fail <| Parser.deadEndsToString deadEnds

                    Ok time ->
                        Decode.succeed time
            )


{-| Encode a `Time.Posix` value as a UTC date string using
[`fromTime`](#fromTime).
-}
encode : Time.Posix -> Encode.Value
encode =
    fromTime >> Encode.string


{-| Convert from a UTC date string to a `Time.Posix` value.

UTC date strings are always GMT. This function returns a time in UTC/GMT.

-}
toTime : String -> Result (List Parser.DeadEnd) Time.Posix
toTime str =
    Parser.run utc str


{-| A fixed-length integer padded with zeroes.
-}
paddedInt : Int -> Parser Int
paddedInt quantity =
    let
        helper str =
            if String.length str == quantity then
                -- StringtoInt works on zero-padded integers
                case String.toInt str of
                    Just intVal ->
                        Parser.succeed intVal
                            |> Parser.map Parser.Done

                    Nothing ->
                        Parser.problem ("Invalid integer: \"" ++ str ++ "\"")

            else
                Parser.chompIf Char.isDigit
                    |> Parser.getChompedString
                    |> Parser.map (\nextChar -> Parser.Loop <| String.append str nextChar)
    in
    Parser.loop "" helper


msPerYear : Int
msPerYear =
    --365 * 24 * 60 * 60 * 1000
    31536000000


msPerDay : Int
msPerDay =
    -- 24 * 60 * 60 * 1000
    86400000


{-| A parsed day was outside the valid month range. (e.g. 0 is never a valid
day in a month, and neither is 32.
-}
invalidDay : Int -> Parser Int
invalidDay day =
    Parser.problem ("Invalid day: " ++ String.fromInt day)


epochYear : Int
epochYear =
    1970


yearMonthDay : ( Int, Month, Int ) -> Parser Int
yearMonthDay ( year, month, dayInMonth ) =
    if dayInMonth < 0 then
        invalidDay dayInMonth

    else
        let
            succeedWith extraMs =
                let
                    days =
                        if (month == Jan || month == Feb) || not (isLeapYear year) then
                            -- If we're in January or February, it doesn't matter
                            -- if we're in a leap year from a days-in-month perspective.
                            -- Only possible impact of leap years in this scenario is
                            -- if we received February 29, which is checked later.
                            -- Also, this doesn't matter if we explicitly aren't
                            -- in a leap year.
                            dayInMonth - 1

                        else
                            -- We're in a leap year in March-December, so add an extra
                            -- day (for Feb 29) compared to what we'd usually do.
                            dayInMonth

                    dayMs =
                        -- one extra day for each leap year
                        msPerDay * (days + (leapYearsBefore year - leapYearsBefore epochYear))

                    yearMs =
                        msPerYear * (year - epochYear)
                in
                Parser.succeed (extraMs + yearMs + dayMs)
        in
        case month of
            Jan ->
                -- 31 days in January
                if dayInMonth > 31 then
                    invalidDay dayInMonth

                else
                    -- Add 0 days when in the first month of the year
                    succeedWith 0

            Feb ->
                -- 28 days in February unless it's a leap year; then 29)
                if (dayInMonth > 29) || (dayInMonth == 29 && not (isLeapYear year)) then
                    invalidDay dayInMonth

                else
                    -- 31 days in January
                    -- (31 * 24 * 60 * 60 * 1000)
                    succeedWith 2678400000

            Mar ->
                -- 31 days in March
                if dayInMonth > 31 then
                    invalidDay dayInMonth

                else
                    -- 28 days in February (leap years are handled elsewhere)
                    -- ((28 + 31) * 24 * 60 * 60 * 1000)
                    succeedWith 5097600000

            Apr ->
                -- 30 days in April
                if dayInMonth > 30 then
                    invalidDay dayInMonth

                else
                    -- 31 days in March
                    -- ((31 + 28 + 31) * 24 * 60 * 60 * 1000)
                    succeedWith 7776000000

            May ->
                -- 31 days in May
                if dayInMonth > 31 then
                    invalidDay dayInMonth

                else
                    -- 30 days in April
                    -- ((30 + 31 + 28 + 31) * 24 * 60 * 60 * 1000)
                    succeedWith 10368000000

            Jun ->
                -- 30 days in June
                if dayInMonth > 30 then
                    invalidDay dayInMonth

                else
                    -- 31 days in May
                    -- ((31 + 30 + 31 + 28 + 31) * 24 * 60 * 60 * 1000)
                    succeedWith 13046400000

            Jul ->
                -- 31 days in July
                if dayInMonth > 31 then
                    invalidDay dayInMonth

                else
                    -- 30 days in June
                    -- ((30 + 31 + 30 + 31 + 28 + 31) * 24 * 60 * 60 * 1000)
                    succeedWith 15638400000

            Aug ->
                -- 31 days in August
                if dayInMonth > 31 then
                    invalidDay dayInMonth

                else
                    -- 31 days in July
                    -- ((31 + 30 + 31 + 30 + 31 + 28 + 31) * 24 * 60 * 60 * 1000)
                    succeedWith 18316800000

            Sep ->
                -- 30 days in September
                if dayInMonth > 30 then
                    invalidDay dayInMonth

                else
                    -- 31 days in August
                    -- ((31 + 31 + 30 + 31 + 30 + 31 + 28 + 31) * 24 * 60 * 60 * 1000)
                    succeedWith 20995200000

            Oct ->
                -- 31 days in October
                if dayInMonth > 31 then
                    invalidDay dayInMonth

                else
                    -- 30 days in September
                    -- ((30 + 31 + 31 + 30 + 31 + 30 + 31 + 28 + 31) * 24 * 60 * 60 * 1000)
                    succeedWith 23587200000

            Nov ->
                -- 30 days in November
                if dayInMonth > 30 then
                    invalidDay dayInMonth

                else
                    -- 31 days in October
                    -- ((31 + 30 + 31 + 31 + 30 + 31 + 30 + 31 + 28 + 31) * 24 * 60 * 60 * 1000)
                    succeedWith 26265600000

            Dec ->
                -- 31 days in December
                if dayInMonth > 31 then
                    invalidDay dayInMonth

                else
                    -- 30 days in November
                    -- ((30 + 31 + 30 + 31 + 31 + 30 + 31 + 30 + 31 + 28 + 31) * 24 * 60 * 60 * 1000)
                    succeedWith 28857600000


fromParts : Int -> Int -> Int -> Int -> Time.Posix
fromParts monthYearDayMs hour minute second =
    Time.millisToPosix
        (monthYearDayMs
            + (hour * 60 * 60 * 1000)
            + (minute * 60 * 1000)
            + (second * 1000)
        )


{-| From <https://www.timeanddate.com/date/leapyear.html>

In the Gregorian calendar three criteria must be taken into account to identify leap years:

  - The year can be evenly divided by 4;
  - If the year can be evenly divided by 100, it is NOT a leap year, unless;
  - The year is also evenly divisible by 400. Then it is a leap year.

This means that in the Gregorian calendar, the years 2000 and 2400 are leap years, while 1800, 1900, 2100, 2200, 2300 and 2500 are NOT leap years.

-}
isLeapYear : Int -> Bool
isLeapYear year =
    (modBy 4 year == 0) && ((modBy 100 year /= 0) || (modBy 400 year == 0))


leapYearsBefore : Int -> Int
leapYearsBefore y1 =
    let
        y =
            y1 - 1
    in
    (y // 4) - (y // 100) + (y // 400)


{-| Www, dd Mmm yyyy hh:mm:ss GMT
-}
utc : Parser Time.Posix
utc =
    -- Www, dd Mmm yyyy
    monthYearDayInMs
        |> andThen
            (\datePart ->
                oneOf
                    [ succeed (fromParts datePart)
                        -- hh
                        |= paddedInt 2
                        |. symbol ":"
                        -- mm
                        |= paddedInt 2
                        |. symbol ":"
                        -- ss
                        |= paddedInt 2
                        |. spaces
                        |. Parser.token "GMT"
                        |. end
                    , succeed (fromParts datePart 0 0 0)
                        |. end
                    ]
            )


{-| Parse the year, month, and day, and convert to milliseconds since the epoch.

We need all three pieces information at once to do this conversion, because of
leap years. Without knowing Month, Year, and Day, we can't tell whether to
succeed or problem when we encounter February 29.

-}
monthYearDayInMs : Parser Int
monthYearDayInMs =
    -- Www, dd Mmm yyyy
    succeed (\day month year -> ( year, month, day ))
        -- Www,
        |. (getAlphaChars |> Parser.andThen checkWeekday)
        |. symbol ","
        |. spaces
        -- dd
        |= paddedInt 2
        |. spaces
        -- Mmm
        |= (getAlphaChars |> Parser.andThen checkMonth)
        |. spaces
        -- yyyy
        |= paddedInt 4
        |. spaces
        |> Parser.andThen yearMonthDay


getAlphaChars : Parser String
getAlphaChars =
    Char.isAlpha
        |> Parser.chompWhile
        |> Parser.getChompedString


checkWeekday : String -> Parser Weekday
checkWeekday weekday =
    case weekday of
        "Mon" ->
            succeed Mon

        "Tue" ->
            succeed Tue

        "Wed" ->
            succeed Wed

        "Thu" ->
            succeed Thu

        "Fri" ->
            succeed Fri

        "Sat" ->
            succeed Sat

        "Sun" ->
            succeed Sun

        _ ->
            Parser.problem ("Invalid weekday: " ++ weekday)


checkMonth : String -> Parser Month
checkMonth month =
    case month of
        "Jan" ->
            succeed Jan

        "Feb" ->
            succeed Feb

        "Mar" ->
            succeed Mar

        "Apr" ->
            succeed Apr

        "May" ->
            succeed May

        "Jun" ->
            succeed Jun

        "Jul" ->
            succeed Jul

        "Aug" ->
            succeed Aug

        "Sep" ->
            succeed Sep

        "Oct" ->
            succeed Oct

        "Nov" ->
            succeed Nov

        "Dec" ->
            succeed Dec

        _ ->
            Parser.problem ("Invalid month: " ++ month)


{-| Inflate a Posix integer into a more memory-intensive UTC date string.

It's generally best to avoid doing this unless an external API requires it.

(UTC integers are less error-prone, take up less memory, and are more efficient
for time arithmetic.)

Format: Www, dd Mmm yyyy hh:mm:ss GMT

-}
fromTime : Time.Posix -> String
fromTime time =
    ---- Www,
    toWeekday time
        ++ ", "
        -- dd
        ++ toPaddedString 2 (Time.toDay Time.utc time)
        ++ " "
        -- Mmm
        ++ fromMonth (Time.toMonth Time.utc time)
        ++ " "
        -- yyyy
        ++ toPaddedString 4 (Time.toYear Time.utc time)
        ++ " "
        -- hh
        ++ toPaddedString 2 (Time.toHour Time.utc time)
        ++ ":"
        -- mm
        ++ toPaddedString 2 (Time.toMinute Time.utc time)
        ++ ":"
        -- ss
        ++ toPaddedString 2 (Time.toSecond Time.utc time)
        ++ " GMT"


toWeekday : Time.Posix -> String
toWeekday time =
    case Time.toWeekday Time.utc time of
        Sun ->
            "Sun"

        Mon ->
            "Mon"

        Tue ->
            "Tue"

        Wed ->
            "Wed"

        Thu ->
            "Thu"

        Fri ->
            "Fri"

        Sat ->
            "Sat"


toPaddedString : Int -> Int -> String
toPaddedString digits time =
    String.padLeft digits '0' (String.fromInt time)


fromMonth : Time.Month -> String
fromMonth month =
    case month of
        Jan ->
            "Jan"

        Feb ->
            "Feb"

        Mar ->
            "Mar"

        Apr ->
            "Apr"

        May ->
            "May"

        Jun ->
            "Jun"

        Jul ->
            "Jul"

        Aug ->
            "Aug"

        Sep ->
            "Sep"

        Oct ->
            "Oct"

        Nov ->
            "Nov"

        Dec ->
            "Dec"
