module Morphir.IR.Path.Fuzzer exposing (..)

import Fuzz exposing (Fuzzer)
import Morphir.IR.Name.Fuzzer exposing (nameFuzzer)
import Morphir.IR.Path exposing (Path)


pathFuzzer : Fuzzer Path
pathFuzzer =
    Fuzz.oneOf
        (List.map pathFuzzerOfLength
            (List.range 1 4)
        )


pathFuzzerOfLength : Int -> Fuzzer Path
pathFuzzerOfLength length =
    if length > 0 then
        Fuzz.map2 (::)
            nameFuzzer
            (pathFuzzerOfLength (length - 1))

    else
        Fuzz.constant []
