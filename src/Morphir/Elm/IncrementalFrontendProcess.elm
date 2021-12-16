module Morphir.Elm.IncrementalFrontendProcess exposing (..)

import Dict exposing (Dict)
import Morphir.Dependency.Dependencies as Dependencies exposing (Dependencies)
import Morphir.Dependency.DependencyGraph as DependencyGraph exposing (DependencyGraph)
import Morphir.Elm.ParsedModule as ParsedModule exposing (ParsedModule)
import Morphir.IR.Module exposing (ModuleName)
import Morphir.IR.Name as Name exposing (Name)
import Morphir.IR.Package exposing (PackageName)
import Morphir.IR.Type as Type
import Morphir.IR.Value as Value
import Set exposing (Set)


type alias ProcessingState =
    { dependencyStates : Dict PackageName PackageState
    , packageState : PackageState
    , dependencyGraph : DependencyGraph
    }


type alias PackageState =
    { packageName : PackageName
    , exposedModules : Set ModuleName
    , modules : Dict ModuleName ModuleState
    }


type ModuleState
    = ModuleSource String
    | ModuleParsed ParsedModule
    | ModuleMapped
        { dependencies : Set ModuleName
        , types : Dict Name (Type.Definition ())
        , values : Dict Name (Value.Definition () ())
        }


type alias SourceFile =
    String


type ProcessingError
    = ProcessingError


type ParseError
    = ParseError


{-| Update the processing state to reflect that some modules sources have changed.
-}
updateSources : Dict ModuleName SourceFile -> ProcessingState -> ProcessingState
updateSources changedSources processingState =
    let
        updatePackageState packageState =
            { packageState
                | modules =
                    packageState.modules
                        |> Dict.map
                            (\moduleName moduleState ->
                                case changedSources |> Dict.get moduleName of
                                    Just source ->
                                        ModuleSource source

                                    Nothing ->
                                        moduleState
                            )
            }
    in
    { processingState
        | packageState = updatePackageState processingState.packageState
    }


process : ProcessingState -> Result ProcessingError ProcessingState
process processingState =
    Ok processingState


moduleSetDependencies : PackageState -> Set ModuleName -> Set ModuleName
moduleSetDependencies packageState moduleNames =
    Set.diff moduleNames
        (moduleNames
            |> Set.toList
            |> List.filterMap
                (\exposedModule ->
                    packageState.modules
                        |> Dict.get exposedModule
                        |> Maybe.map (moduleDependencies packageState.packageName)
                )
            |> List.foldl Set.union Set.empty
        )


moduleDependencies : PackageName -> ModuleState -> Set ModuleName
moduleDependencies packageName moduleState =
    case moduleState of
        ModuleSource _ ->
            Set.empty

        ModuleParsed parsedModule ->
            parsedModule
                |> ParsedModule.localModuleDependencies packageName

        ModuleMapped m ->
            m.dependencies
