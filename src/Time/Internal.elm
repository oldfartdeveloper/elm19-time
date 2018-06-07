module Time.Internal exposing (..)


type alias DateTimeData =
    { year : Int
    , month : Int
    , day : Int
    , hour : Int
    , minute : Int
    , second : Int
    , millisecond : Int
    }


type alias TimeData d =
    { d
        | hour : Int
        , minute : Int
        , second : Int
        , millisecond : Int
    }


offsetFromTimeData : TimeData d -> Int
offsetFromTimeData { hour, minute, second, millisecond } =
    clamp 0 23 hour * hourMs + clamp 0 59 minute * minuteMs + clamp 0 59 second * secondMs + clamp 0 999 millisecond


zero : DateTimeData
zero =
    { year = 0
    , month = 1
    , day = 1
    , hour = 0
    , minute = 0
    , second = 0
    , millisecond = 0
    }


padded : Int -> String
padded n =
    if n < 10 then
        "0" ++ String.fromInt n

    else
        String.fromInt n


padded3 : Int -> String
padded3 n =
    String.padLeft 3 '0' (String.fromInt n)


dayMs : Int
dayMs =
    86400000


hourMs : Int
hourMs =
    3600000


minuteMs : Int
minuteMs =
    60000


secondMs : Int
secondMs =
    1000
