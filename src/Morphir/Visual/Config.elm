module Morphir.Visual.Config exposing (Config, EventHandlers, IRContext, PopupScreenRecord, VisualState, evaluate)

import Dict exposing (Dict)
import Morphir.IR as IR exposing (IR)
import Morphir.IR.Distribution exposing (Distribution)
import Morphir.IR.FQName exposing (FQName)
import Morphir.IR.Name exposing (Name)
import Morphir.IR.Type exposing (Type)
import Morphir.IR.Value as Value exposing (RawValue, TypedValue)
import Morphir.Value.Interpreter as Interpreter
import Morphir.Value.Native as Native
import Morphir.Visual.Theme exposing (Theme)
import Set exposing (Set)


type alias Config msg =
    { irContext : IRContext
    , state : VisualState
    , handlers : EventHandlers msg
    }


type alias IRContext =
    { ir : IR
    , distribution : Distribution
    , nativeFunctions : Dict FQName Native.Function
    }


type alias VisualState =
    { expandedFunctions : Dict FQName (Value.Definition () (Type ()))
    , variables : Dict Name RawValue
    , popupVariables : PopupScreenRecord
    , theme : Theme
    , valuesToEvaluate : Set Int
    }


type alias EventHandlers msg =
    { onReferenceClicked : FQName -> Bool -> msg
    , onHoverOver : Int -> Maybe RawValue -> msg
    , onHoverLeave : Int -> msg
    , onClick : Int -> msg
    }


type alias PopupScreenRecord =
    { variableIndex : Int
    , variableValue : Maybe RawValue
    }


evaluate : RawValue -> Config msg -> Result String RawValue
evaluate value config =
    Interpreter.evaluateValue config.irContext.nativeFunctions (IR.fromDistribution config.irContext.distribution) config.state.variables [] value
        |> Result.mapError
            (\error ->
                error
                    |> Debug.log (String.concat [ "Error while evaluating '", Debug.toString value, "'" ])
                    |> Debug.toString
            )
