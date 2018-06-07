module Time.Date
    exposing
        ( Date
        , DateDelta
        , addDays
        , addMonths
        , addYears
        , compare
        , date
        , day
        , daysInMonth
        , delta
        , fromTuple
        , isLeapYear
        , isValidDate
        , month
        , setDay
        , setMonth
        , setYear
        , toTuple
        , weekday
        , year
        )

{-| This module defines a timezone-independent Date type which can
represent any date of the proleptic Gregorian calendar.


# Dates

@docs Date


# Constructing Dates

@docs date, fromTuple, toTuple


# Inspecting Dates

@docs year, month, day, Weekday, weekday


# Manipulating Dates

@docs setYear, setMonth, setDay, addYears, addMonths, addDays


# Comparing Dates

@docs compare, DateDelta, delta


# Helper functions

@docs isValidDate, isLeapYear, daysInMonth

-}

import Time exposing (Weekday(..))


{-| Date is the opaque type for all Date values. Values of this type
are guaranteed to represent valid proleptic Gregorian calendar dates.
-}
type Date
    = Date
        { opaqueYear : Int
        , opaqueMonth : Int
        , opaqueDay : Int
        }


{-| DateDelta represents a delta between two dates.
-}
type alias DateDelta =
    { years : Int
    , months : Int
    , days : Int
    }


{-| date constructs a Date value given a year, a month and a day.
Invalid values are clamped to the nearest valid date.

    d : Date
    d =
        date 2018 5 29

    year d --> 2018
    month d --> 5
    day d --> 29

-}
date : Int -> Int -> Int -> Date
date yr mo dy =
    firstValid yr (clampMonth mo) (clampDay dy)


{-| year returns a Date's year as an Int.

    year (date 2018 5 26)
    --> 2018

-}
year : Date -> Int
year (Date { opaqueYear }) =
    opaqueYear


{-| month returns a Date's month as an Int. Guaranteed to be in the
range [1, 12].

    month (date 2018 13 26) -- Note month will be clamped
    --> 12

-}
month : Date -> Int
month (Date { opaqueMonth }) =
    opaqueMonth


{-| day returns a Date's year as an Int. Guaranteed to be valid for
the Date's (year, month) pair and in the range [1, 31].

    day (date 2018 2 28)
    --> 28

    day (date 2018 2 29)
    --> 28    -- observed clamped

    day (date 2000 2 29)
    --> 29    -- leap year

-}
day : Date -> Int
day (Date { opaqueDay }) =
    opaqueDay


{-| weekday returns the day of week for a given Date.

This uses Sakamoto's method to determine the day of week.

    weekday (date 2018 5 26)
    --> Sat

-}
weekday : Date -> Weekday
weekday (Date { opaqueYear, opaqueMonth, opaqueDay }) =
    let
        m =
            if opaqueMonth == 1 then
                0

            else if opaqueMonth == 2 then
                3

            else if opaqueMonth == 3 then
                2

            else if opaqueMonth == 4 then
                5

            else if opaqueMonth == 5 then
                0

            else if opaqueMonth == 6 then
                3

            else if opaqueMonth == 7 then
                5

            else if opaqueMonth == 8 then
                1

            else if opaqueMonth == 9 then
                4

            else if opaqueMonth == 10 then
                6

            else if opaqueMonth == 11 then
                2

            else
                4

        y =
            if opaqueMonth < 3 then
                opaqueYear - 1

            else
                opaqueYear

        d =
            modBy 7 (y + y // 4 - y // 100 + y // 400 + m + opaqueDay)
    in
    if d == 0 then
        Sun

    else if d == 1 then
        Mon

    else if d == 2 then
        Tue

    else if d == 3 then
        Wed

    else if d == 4 then
        Thu

    else if d == 5 then
        Fri

    else
        Sat


{-| setYear updates a Date's year. Invalid values are clamped to the
nearest valid date.

    date 2000 5 26
    |> setYear 2016
    |> year
    --> 2016

-}
setYear : Int -> Date -> Date
setYear yr (Date { opaqueMonth, opaqueDay }) =
    firstValid yr opaqueMonth opaqueDay


{-| setMonth updates a Date's month. Invalid values are clamped to the
nearest valid date.

    date 2016 5 26
    |> setMonth 6
    |> month
    --> 6

    date 2016 5 26
    |> setMonth 13 -- will be clamped
    |> month
    --> 12

-}
setMonth : Int -> Date -> Date
setMonth mo (Date { opaqueYear, opaqueDay }) =
    firstValid opaqueYear (clampMonth mo) opaqueDay


{-| setDay updates a Date's day. Invalid values are clamped to the
nearest valid date.

    date 2016 2 26
    |> setDay 28
    |> day
    --> 28

    date 2016 2 28
    |> setDay 29    -- leap year
    |> day
    --> 29

    date 2015 2 28
    |> setDay 29    -- clamped
    |> day
    --> 28

-}
setDay : Int -> Date -> Date
setDay dy (Date { opaqueYear, opaqueMonth }) =
    firstValid opaqueYear opaqueMonth (clampDay dy)


{-| addYears adds a relative number (positive or negative) of years to
a Date, ensuring that the return value represents a valid Date. If
the new date is not valid, days are subtracted from it until a valid
Date can be produced.

    date 2000 2 29
    |> addYears -1  -- will no longer be leap year
    |> day
    --> 28

-}
addYears : Int -> Date -> Date
addYears years (Date { opaqueYear, opaqueMonth, opaqueDay }) =
    firstValid (opaqueYear + years) opaqueMonth opaqueDay


{-| addMonths adds a relative number (positive or negative) of months to
a Date, ensuring that the return value represents a valid Date. Its
semantics are the same as `addYears`.

    date 2018 3 31
    |> addMonths -1 -- Switch to Feb
    |> day
    --> 28

-}
addMonths : Int -> Date -> Date
addMonths months (Date { opaqueYear, opaqueMonth, opaqueDay }) =
    let
        ms =
            opaqueYear * 12 + opaqueMonth - 1 + months

        yo =
            if ms < 0 then
                -1

            else
                0
    in
    date (((ms - yo) // 12) + yo) (modBy 12 ms + 1) opaqueDay


{-| days adds an exact number (positive or negative) of days to a
Date. Adding or subtracting days always produces a valid Date so
there is no fuzzing logic here like there is in `add{Months,Years}`.

    date 2018 2 28
    |> addDays 1
    |> month
    --> 3 -- March

-}
addDays : Int -> Date -> Date
addDays days (Date { opaqueYear, opaqueMonth, opaqueDay }) =
    daysFromYearMonthDay opaqueYear opaqueMonth opaqueDay
        |> (+) days
        |> dateFromDays


{-| compare two Dates.

Note: since this conflicts with **Basics.compare**, have to
preface with **Time.Date.**; see this example:

    date 2018 1 28
    |> addYears -1
    |> addMonths 1
    |> Time.Date.compare (date 2017 2 29)
    --> EQ

-}
compare : Date -> Date -> Order
compare d1 d2 =
    Basics.compare (toTuple d1) (toTuple d2)


{-| delta returns the relative number of years, months and days between two Dates.

Each field is accumulative by itself. That is, `days` not only shows the
difference caused by the two `day` entries in the two `Date` arguments, but
also the added days caused by differences in `months` and `years`. For `months`
and `years`, is the count across month and year change boundaries respectively; illustrated
by last example below.

    -- 3 examples showing that, if the `year` and `month`
    -- are the same in the two `Date` values, then the
    -- `years` and `months` result values remain constant
    -- in spite of large differences in the two inputs'
    -- `day` setting:

    delta (date 2019 1 1) (date 2018 1 1)
    --> { years = 1
    --> , months = 12
    --> , days = 365
    --> }

    delta (date 2019 1 31) (date 2018 1 1)
    --> { years = 1
    --> , months = 12
    --> , days = 395
    --> }

    delta (date 2019 1 1) (date 2018 1 31)
    --> { years = 1
    --> , months = 12
    --> , days = 335
    --> }

    -- 1 day apart but from last day of year to first
    -- day of next year:

    delta (date 2019 1 1) (date 2018 12 31)
    --> { years = 1
    --> , months = 1
    --> , days = 1
    --> }

-}
delta : Date -> Date -> DateDelta
delta (Date d1) (Date d2) =
    { years = d1.opaqueYear - d2.opaqueYear
    , months = (abs d1.opaqueYear * 12 + d1.opaqueMonth) - (abs d2.opaqueYear * 12 + d2.opaqueMonth)
    , days =
        daysFromYearMonthDay d1.opaqueYear d1.opaqueMonth d1.opaqueDay
            - daysFromYearMonthDay d2.opaqueYear d2.opaqueMonth d2.opaqueDay
    }


{-| toTuple converts a Date value into a (year, month, day) tuple.
This is useful if you want to use Dates as Dict keys.

    date 2018 5 26
    |> toTuple
    --> (2018, 5, 26)

-}
toTuple : Date -> ( Int, Int, Int )
toTuple (Date { opaqueYear, opaqueMonth, opaqueDay }) =
    ( opaqueYear, opaqueMonth, opaqueDay )


{-| fromTuple converts a (year, month, day) tuple into a Date value.

    (2018, 5, 26)
    |> fromTuple
    --> date 2018 5 26

-}
fromTuple : ( Int, Int, Int ) -> Date
fromTuple ( opaqueYear, opaqueMonth, opaqueDay ) =
    date opaqueYear opaqueMonth opaqueDay


{-| isValidDate returns True if the given year, month and day
represent a valid date.

NOTE: when you create a Date using `date`, it does not validate
the `year`, `month`, or `day` used; rather it just clamps out-of-range
values to "legal" values without notifying you. If you are worried
about complete validation, pass the 3 values to this
method first and it will validate it. This gives you a chance to
abort creating a "bad" `Date`.

    isValidDate 2016 12 31
    --> True

    isValidDate 2016 12 32
    --> False

    isValidDate 2016 2 29 -- leap year
    --> True

    isValidDate 2018 2 29 -- not leap year
    --> False

-}
isValidDate : Int -> Int -> Int -> Bool
isValidDate y m d =
    daysInMonth y m
        |> Maybe.map (\days -> d >= 1 && d <= days)
        |> Maybe.withDefault False


{-| isLeapYear returns True if the given year is a leap year. The
rules for leap years are as follows:

  - A year that is a multiple of 400 is a leap year.

  - A year that is a multiple of 100 but not of 400 is not a leap year.

  - A year that is a multiple of 4 but not of 100 is a leap year.

    isLeapYear 2016
    --> True

    isLeapYear 2018
    --> False

    isLeapYear 400
    --> True

    isLeapYear 500
    --> False

    isLeapYear (500 + 4)
    --> True

-}
isLeapYear : Int -> Bool
isLeapYear y =
    modBy 400 y == 0 || modBy 100 y /= 0 && modBy 4 y == 0


{-| daysInMonth returns the number of days in a month given a specific
year, taking leap years into account.

  - A regular year has 365 days and the corresponding February has 28 days.

  - A leap year has 366 days and the corresponding February has 29 days.

    daysInMonth 2016 2
    --> Just 29

    daysInMonth 2018 2
    --> Just 28

    daysInMonth 2018 13 -- month out of range
    --> Nothing

-}
daysInMonth : Int -> Int -> Maybe Int
daysInMonth y m =
    if m >= 1 && m <= 12 then
        Just <| unsafeDaysInMonth y m

    else
        Nothing


unsafeDaysInMonth : Int -> Int -> Int
unsafeDaysInMonth y m =
    if m == 1 then
        31

    else if m == 2 && isLeapYear y then
        29

    else if m == 2 then
        28

    else if m == 3 then
        31

    else if m == 4 then
        30

    else if m == 5 then
        31

    else if m == 6 then
        30

    else if m == 7 then
        31

    else if m == 8 then
        31

    else if m == 9 then
        30

    else if m == 10 then
        31

    else if m == 11 then
        30

    else if m == 12 then
        31

    else
        -- FIXME: Debug.crash isn't available in Elm 0.19
        Debug.todo ("handle invalid call to unsafeDaysInMonth: year=" ++ String.fromInt y ++ " month=" ++ String.fromInt m)
            31


firstValid : Int -> Int -> Int -> Date
firstValid yr mo dy =
    let
        ( y, m, d ) =
            if isValidDate yr mo dy then
                ( yr, mo, dy )

            else if isValidDate yr mo (dy - 1) then
                ( yr, mo, dy - 1 )

            else if isValidDate yr mo (dy - 2) then
                ( yr, mo, dy - 2 )

            else
                ( yr, mo, dy - 3 )
    in
    Date { opaqueYear = y, opaqueMonth = m, opaqueDay = d }


daysFromYearMonthDay : Int -> Int -> Int -> Int
daysFromYearMonthDay y m d =
    let
        yds =
            daysFromYear y

        mds =
            daysFromYearMonth y m

        dds =
            d - 1
    in
    yds + mds + dds


daysFromYearMonth : Int -> Int -> Int
daysFromYearMonth yr mo =
    let
        go y m acc =
            if m == 0 then
                acc

            else
                go y (m - 1) (acc + unsafeDaysInMonth y m)
    in
    go yr (mo - 1) 0


daysFromYear : Int -> Int
daysFromYear y =
    if y > 0 then
        366
            + ((y - 1) * 365)
            + ((y - 1) // 4)
            - ((y - 1) // 100)
            + ((y - 1) // 400)

    else if y < 0 then
        (y * 365)
            + (y // 4)
            - (y // 100)
            + (y // 400)

    else
        0


yearFromDays : Int -> Int
yearFromDays ds =
    let
        y =
            ds // 365

        d =
            daysFromYear y
    in
    if ds <= d then
        y - 1

    else
        y


dateFromDays : Int -> Date
dateFromDays ds =
    let
        d400 =
            daysFromYear 400

        y400 =
            ds // d400

        d =
            remainderBy d400 ds

        year_ =
            yearFromDays (d + 1)

        leap =
            if isLeapYear year_ then
                (+) 1

            else
                identity

        doy =
            d - daysFromYear year_

        ( month_, day_ ) =
            if doy < 31 then
                ( 1, doy + 1 )

            else if doy < leap 59 then
                ( 2, doy - 31 + 1 )

            else if doy < leap 90 then
                ( 3, doy - leap 59 + 1 )

            else if doy < leap 120 then
                ( 4, doy - leap 90 + 1 )

            else if doy < leap 151 then
                ( 5, doy - leap 120 + 1 )

            else if doy < leap 181 then
                ( 6, doy - leap 151 + 1 )

            else if doy < leap 212 then
                ( 7, doy - leap 181 + 1 )

            else if doy < leap 243 then
                ( 8, doy - leap 212 + 1 )

            else if doy < leap 273 then
                ( 9, doy - leap 243 + 1 )

            else if doy < leap 304 then
                ( 10, doy - leap 273 + 1 )

            else if doy < leap 334 then
                ( 11, doy - leap 304 + 1 )

            else
                ( 12, doy - leap 334 + 1 )
    in
    Date
        { opaqueYear = year_ + y400 * 400
        , opaqueMonth = month_
        , opaqueDay = day_
        }


clampMonth : Int -> Int
clampMonth cm =
    clamp 1 12 cm


clampDay : Int -> Int
clampDay cd =
    clamp 1 31 cd
