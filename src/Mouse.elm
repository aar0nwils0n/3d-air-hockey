module Mouse exposing (MouseMove, decoder)

import Json.Decode as Decode exposing (..)


type alias MouseMove =
    { clientX : Int
    , clientY : Int
    }


decoder =
    map2 MouseMove
        (field "clientX" int)
        (field "clientY" int)
