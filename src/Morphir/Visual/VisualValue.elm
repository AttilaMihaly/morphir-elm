module Morphir.Visual.VisualValue exposing (..)

import Morphir.IR.Type exposing (Type)
import Morphir.IR.Value as Value exposing (TypedValue, Value)


type alias VisualValue =
    Value () ( Int, Type () )


fromTypedValue : TypedValue -> VisualValue
fromTypedValue typedValue =
    typedValue
        |> Value.indexedMapValue Tuple.pair 0
        |> Tuple.first
