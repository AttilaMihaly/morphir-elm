module Morphir.Dependency.DependencyGraph exposing (..)

import Dict exposing (Dict)
import Morphir.Dependency.Dependencies exposing (Dependencies)
import Morphir.IR.FQName exposing (FQName)
import Morphir.IR.Module exposing (ModuleName)
import Morphir.IR.Package exposing (PackageName)
import Set exposing (Set)


type alias QualifiedModuleName =
    ( PackageName, ModuleName )


type alias DependencyGraph =
    { packages : Dependencies PackageName
    , modules : Dependencies QualifiedModuleName
    , types : Dependencies FQName
    , values : Dependencies FQName
    }


moduleDependsOn : QualifiedModuleName -> Set QualifiedModuleName -> DependencyGraph -> DependencyGraph
moduleDependsOn moduleName dependsOn graph =
    { graph
        | modules =
            graph.modules
    }


focusOnPackageModules : PackageName -> DependencyGraph -> Dependencies ModuleName
focusOnPackageModules =
    Debug.todo "implement"


transitiveModuleDependencies : DependencyGraph -> Set QualifiedModuleName -> Set QualifiedModuleName
transitiveModuleDependencies graph moduleNames =
    if Set.isEmpty moduleNames then
        Set.empty

    else
        transitiveModuleDependencies
            graph
            (moduleNames
                |> setUnionMap
                    (\moduleName ->
                        graph.modules.dependsOn
                            |> Dict.get moduleName
                            |> Maybe.withDefault Set.empty
                    )
            )


setUnionMap : (comparable1 -> Set comparable2) -> Set comparable1 -> Set comparable2
setUnionMap f set =
    set
        |> Set.toList
        |> List.map f
        |> List.foldl Set.union Set.empty


setFilterMap : (comparable1 -> Maybe comparable2) -> Set comparable1 -> Set comparable2
setFilterMap f set =
    set
        |> Set.foldl
            (\maybeComp2 resultSoFar ->
                case f maybeComp2 of
                    Just comp2 ->
                        resultSoFar |> Set.insert comp2

                    Nothing ->
                        resultSoFar
            )
            Set.empty
