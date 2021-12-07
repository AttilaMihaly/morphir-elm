module Morphir.Elm.IncrementalFrontendProcess exposing (..)

import Dict exposing (Dict)
import Morphir.IR.Module exposing (ModuleName)
import Morphir.IR.Name exposing (Name)
import Morphir.IR.Package exposing (PackageName)
import Morphir.IR.Type as Type
import Morphir.IR.Value as Value
import Set exposing (Set)


type alias ProcessingState =
    { sources : Dict ModuleName SourceState
    , exposedModules : Set ModuleName
    , dependencyStates : Dict PackageName PackageState
    , packageState : PackageState
    }


type alias PackageState =
    { modules : Dict ModuleName ModuleState
    }


type alias ModuleState =
    { dependentModules : Set ModuleName
    , types : Dict Name (Type.Definition ())
    , values : Dict Name (Value.Definition () ())
    }


type alias SourceFile =
    {}


type SourceState
    = ElmSource


type ProcessingError
    = ProcessingError


type ParseError
    = ParseError


updateSources : Dict ModuleName SourceFile -> ProcessingState -> Result ProcessingError ProcessingState
updateSources changedSources processingState =
    if Dict.isEmpty changedSources then
        Ok processingState

    else
        Debug.todo "implement"


{-| Remove a set of modules and all its dependents.
-}
removePackageModules : Set ModuleName -> PackageState -> PackageState
removePackageModules modulesToRemove packageState =
    modulesToRemove
        |> Set.toList
        |> List.map (\moduleName -> dependentModules moduleName packageState)
        |> List.foldl Set.union Set.empty
        |> Set.foldl
            (\moduleToRemove packageStateSoFar ->
                { packageStateSoFar
                    | modules =
                        packageStateSoFar.modules
                            |> Dict.remove moduleToRemove
                }
            )
            packageState


dependentModules : ModuleName -> PackageState -> Set ModuleName
dependentModules moduleName packageState =
    packageState.modules
        |> Dict.get moduleName
        |> Maybe.map .dependentModules
        |> Maybe.withDefault Set.empty


parseSourceFile : SourceFile -> Result ParseError SourceState
parseSourceFile sourceFile =
    Debug.todo "implement"
