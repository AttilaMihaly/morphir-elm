module Morphir.ADL.Model.Types exposing (Either(..), Map, MapEntry, Maybe(..), Pair, Result(..), Set)


type alias Pair t1 t2 =
    { v1 : t1
    , v2 : t2
    }


type Either t1 t2
    = Left t1
    | Right t2


type Maybe t
    = Nothing
    | Just t


type Result t e
    = Ok t
    | Error e


type alias MapEntry k v =
    { key : k
    , value : v
    }


type alias Map k v =
    List (MapEntry k v)


type alias Set t =
    List t
