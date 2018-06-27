module Date.Distance.I18n.Nl
    exposing
        ( LocaleConfig
        , locale
        )

{-| Dutch locale.
@docs LocaleConfig
@docs locale
-}

import Date.Distance.Types exposing (DistanceLocale(..), Locale)
import Date.Extra exposing (Interval(..))


{-| Configure the localization function.

  - `addSuffix` – turns `2 dagen` into `2 dagen geleden` or `over 2 dagen`

-}
type alias LocaleConfig =
    { addSuffix : Bool }


{-| Configure the Dutch locale.

    locale =
        I18n.Nl.locale { addSuffix = True }

    inWords =
        { defaultConfig | locale = locale }
            |> inWordsWithConfig

-}
locale : LocaleConfig -> Locale
locale { addSuffix } order distance =
    let
        result =
            locale_ distance
    in
    if addSuffix then
        if order == LT then
            "over " ++ result
        else
            result ++ " geleden"
    else
        result


locale_ : DistanceLocale -> String
locale_ distance =
    case distance of
        LessThanXSeconds i ->
            circa "minder dan" Second i

        HalfAMinute ->
            "een halve minuut"

        LessThanXMinutes i ->
            circa "minder dan" Minute i

        XMinutes i ->
            exact Minute i

        AboutXHours i ->
            circa "ongeveer" Hour i

        XDays i ->
            exact Day i

        AboutXMonths i ->
            circa "ongeveer" Month i

        XMonths i ->
            exact Month i

        AboutXYears i ->
            circa "ongeveer" Year i

        OverXYears i ->
            circa "meer dan" Year i

        AlmostXYears i ->
            circa "bijna" Year i


formatInterval : Interval -> String
formatInterval interval =
    case interval of
        Millisecond ->
            "milliseconde"

        Second ->
            "seconde"

        Minute ->
            "minuut"

        Hour ->
            "uur"

        Day ->
            "dag"

        Week ->
            "week"

        Month ->
            "maand"

        Year ->
            "jaar"

        Quarter ->
            "kwartaal"

        Monday ->
            "maandag"

        Tuesday ->
            "dinsdag"

        Wednesday ->
            "woensdag"

        Thursday ->
            "donderdag"

        Friday ->
            "vrijdag"

        Saturday ->
            "zaterdag"

        Sunday ->
            "zondag"


singular : Interval -> String
singular interval =
    case interval of
        Minute ->
            "een " ++ formatInterval interval

        _ ->
            "1 " ++ formatInterval interval


pluralizeInterval : Interval -> String
pluralizeInterval interval =
    case interval of
        Millisecond ->
            "milliseconden"

        Second ->
            "seconden"

        Minute ->
            "minuten"

        Hour ->
            "uur"

        Week ->
            "weken"

        Year ->
            "jaar"

        Quarter ->
            "kwartalen"

        _ ->
            formatInterval interval ++ "en"


circa : String -> Interval -> Int -> String
circa prefix interval i =
    case i of
        1 ->
            prefix ++ " " ++ singular interval

        _ ->
            prefix ++ " " ++ toString i ++ " " ++ pluralizeInterval interval


exact : Interval -> Int -> String
exact interval i =
    case i of
        1 ->
            "1 " ++ formatInterval interval

        _ ->
            toString i ++ " " ++ pluralizeInterval interval
