module Morphir.Elm.ParsedModule exposing (..)

import Elm.RawFile as RawFile exposing (RawFile)
import Elm.Syntax.Node as Node
import Morphir.Elm.ElmModuleName exposing (ElmModuleName)
import Morphir.IR.Name as Name


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
