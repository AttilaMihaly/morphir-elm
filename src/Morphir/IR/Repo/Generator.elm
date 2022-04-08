module Morphir.IR.Repo.Generator exposing (..)

import Morphir.IR.Package exposing (PackageName)
import Morphir.IR.Path.Generator exposing (randomPath)
import Morphir.IR.Repo as Repo exposing (Repo)
import Random exposing (Generator)


randomRepo : Generator Repo
randomRepo =
    randomPath
        |> Random.andThen randomRepoWithPackageName


randomRepoWithPackageName : PackageName -> Generator Repo
randomRepoWithPackageName packageName =
    Random.constant (Repo.empty packageName)
