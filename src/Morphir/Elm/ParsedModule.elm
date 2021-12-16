module Morphir.Elm.ParsedModule exposing (..)

import Elm.RawFile as RawFile exposing (RawFile)
import Elm.Syntax.Node as Node
import Morphir.Elm.ElmModuleName exposing (ElmModuleName)
import Morphir.IR.Module exposing (ModuleName)
import Morphir.IR.Name as Name
import Morphir.IR.Package exposing (PackageName)
import Morphir.IR.Path as Path
import Set exposing (Set)


type ParsedModule
    = ParsedModule RawFile


fromRawFile : RawFile -> ParsedModule
fromRawFile rawFile =
    ParsedModule rawFile


toRawFile : ParsedModule -> RawFile
toRawFile (ParsedModule rawFile) =
    rawFile


moduleName : ParsedModule -> ElmModuleName
moduleName (ParsedModule rawFile) =
    RawFile.moduleName rawFile


importedModules : ParsedModule -> List ElmModuleName
importedModules (ParsedModule rawFile) =
    rawFile
        |> RawFile.imports
        |> List.map (.moduleName >> Node.value)


localModuleDependencies : PackageName -> ParsedModule -> Set ModuleName
localModuleDependencies packageName parsedModules =
    importedModules parsedModules
        |> List.filterMap
            (\elmModuleName ->
                let
                    fullModuleName =
                        elmModuleName |> List.map Name.fromString
                in
                if packageName |> Path.isPrefixOf fullModuleName then
                    Just (fullModuleName |> List.drop (List.length fullModuleName))

                else
                    Nothing
            )
        |> Set.fromList
