module Morphir.IR.Path.Generator exposing (..)

import Morphir.IR.Name.Generator exposing (randomName)
import Morphir.IR.Path exposing (Path)
import Random exposing (Generator)


randomPath : Generator Path
randomPath =
    Random.andThen randomPathOfLength
        (Random.int 1 4)


randomPathOfLength : Int -> Generator Path
randomPathOfLength length =
    Random.list length randomName
