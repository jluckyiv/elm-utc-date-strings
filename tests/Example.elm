module Example exposing (knownValues, reflexive)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, float, int, list, string)
import Test exposing (..)
import Time
import Utc


knownValues : Test
knownValues =
    describe "Epoch"
        [ test "fromTime 0 is January 1, 1970 at midnight" <|
            \_ ->
                Utc.fromTime (Time.millisToPosix 0)
                    |> Expect.equal "Thu, 01 Jan 1970 00:00:00 GMT"
        , test "toTime \"Thu, 01 Jan 1970 00:00:00 GMT\" gives me 0" <|
            \_ ->
                Utc.toTime "Thu, 01 Jan 1970 00:00:00 GMT"
                    |> Expect.equal (Ok (Time.millisToPosix 0))
        , test "toTime \"Wed, 01 Apr 2012 00:00:00 GMT\" gives me 1333256400000" <|
            \_ ->
                Utc.toTime "Sun, 01 Apr 2012 05:00:00 GMT"
                    |> Expect.equal (Ok (Time.millisToPosix 1333256400000))
        , test "toTime \"Sun, 11 Nov 2012 23:00:00 GMT\" gives me 1352674800000" <|
            \_ ->
                Utc.toTime "Sun, 11 Nov 2012 23:00:00 GMT"
                    |> Expect.equal (Ok (Time.millisToPosix 1352674800000))
        , test "toTime \"Sun, 12 Nov 2012 00:00:00 GMT\" gives me 1352678400000" <|
            \_ ->
                Utc.toTime "Sun, 12 Nov 2012 00:00:00 GMT"
                    |> Expect.equal (Ok (Time.millisToPosix 1352678400000))
        , test "Invalid timestamps don't parse" <|
            \_ ->
                Utc.toTime "Sun, 01 Apr 2012 05:basketball"
                    |> Expect.err
        , test "toTime fails with no delimiters and not enough numbers" <|
            \_ ->
                Utc.toTime "Wed,14Jun201012010"
                    |> Expect.err
        ]


reflexive : Test
reflexive =
    fuzz int "(fromTime >> toTime) is a no-op" <|
        \num ->
            let
                seconds =
                    (*) 1000

                time =
                    Time.millisToPosix (num |> seconds)
            in
            Utc.fromTime time
                |> Utc.toTime
                |> Expect.equal (Ok time)
