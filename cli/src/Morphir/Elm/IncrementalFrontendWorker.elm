{-
   Copyright 2020 Morgan Stanley

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
-}


port module Morphir.Elm.IncrementalFrontendWorker exposing (main)

{-| This is a worker for the incremental Elm frontend. It is designed to be stateful to allow users to make incremental
changes to an existing IR.
-}

import Dict exposing (Dict)
import Json.Decode as Decode
import Json.Encode as Encode
import Morphir.Elm.ElmModuleName exposing (ElmModuleName)
import Morphir.Elm.IncrementalFrontend as IncrementalFrontend exposing (PackageError)
import Morphir.Elm.IncrementalFrontend.Codec exposing (encodePackageError, encodeParseError)
import Morphir.Elm.ParsedModule as ParsedModule exposing (ParsedModule)
import Morphir.IR as IR exposing (IR)
import Morphir.IR.Distribution exposing (Distribution)
import Morphir.IR.Distribution.Codec exposing (decodeVersionedDistribution, encodeVersionedDistribution)
import Morphir.IR.Module exposing (ModuleName)
import Morphir.IR.Name as Name
import Morphir.IR.Package as Package exposing (PackageName)
import Morphir.IR.Path as Path
import Morphir.IR.SDK as SDK
import Morphir.SDK.ResultList as ResultList
import Set exposing (Set)
import Time exposing (Posix)


{-| Main entry point for the worker.

We declare the flags as `Decode.Value` to have full control over the serialization but we also define a `Flags` type
below to make the structure clear.

-}
main : Platform.Program Decode.Value Model Msg
main =
    Platform.worker
        { init = init
        , update = update
        , subscriptions = subscriptions
        }


port reportError : String -> Cmd msg


port reportParseError : Encode.Value -> Cmd msg


port reportPackageError : Encode.Value -> Cmd msg


port insertModuleSources : (Encode.Value -> msg) -> Sub msg


port insertModuleSourcesComplete : Encode.Value -> Cmd msg


{-| Represents the inputs passed to the worker at startup.
-}
type alias Flags =
    { packageName : PackageName
    , exposedModules : Set ModuleName
    , existingDistribution : Maybe Distribution
    }


{-| Type that represents the sate of the worker. Currently we store the following states:

  - ir: the current state of the IR, may be an empty IR in the beginning or not-empty if there was one passed in through
    flags
  - parsedModules: modules that have already been parsed but not added to the IR

-}
type alias Model =
    { packageName : PackageName
    , exposedModules : Set ModuleName
    , ir : IR
    , parsedModules : Dict ElmModuleName ParsedModule
    }


{-| Type used to describe module source files.
-}
type alias ModuleSource =
    { path : String
    , modifiedAt : Posix
    , content : String
    }


{-| Dependencies that are added by default without explicit reference.
-}
defaultDependencies : Dict PackageName (Package.Specification ())
defaultDependencies =
    Dict.fromList
        [ ( SDK.packageName, SDK.packageSpec )
        ]


{-| Initialize the state of the worker. It takes the existing distribution from the flags if available, otherwise
initializes to an empty IR.
-}
init : Decode.Value -> ( Model, Cmd Msg )
init flagsJson =
    let
        defaultIR : IR
        defaultIR =
            IR.fromPackageSpecifications defaultDependencies
    in
    case flagsJson |> Decode.decodeValue decodeFlags of
        Ok flags ->
            ( { packageName =
                    flags.packageName
              , exposedModules =
                    flags.exposedModules
              , ir =
                    flags.existingDistribution
                        |> Maybe.map IR.fromDistribution
                        |> Maybe.withDefault IR.empty
                        |> IR.union defaultIR
              , parsedModules =
                    Dict.empty
              }
            , Cmd.none
            )

        Err error ->
            ( { packageName = [ [ "not", "loaded" ] ]
              , ir = IR.empty
              , exposedModules = Set.empty
              , parsedModules = Dict.empty
              }
            , reportError ("Error while decoding Flags: " ++ Decode.errorToString error)
            )


type Msg
    = InsertModuleSources Encode.Value


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InsertModuleSources moduleSourcesJson ->
            case moduleSourcesJson |> Decode.decodeValue (Decode.list decodeModuleSource) of
                Ok moduleSources ->
                    case
                        moduleSources
                            |> List.map (.content >> IncrementalFrontend.parseModule)
                            |> ResultList.keepAllErrors
                    of
                        Ok latestParsedModules ->
                            let
                                latestParsedModulesByName : Dict ElmModuleName ParsedModule
                                latestParsedModulesByName =
                                    latestParsedModules
                                        |> List.filterMap
                                            (\parsedModule ->
                                                let
                                                    elmModuleName : ElmModuleName
                                                    elmModuleName =
                                                        ParsedModule.moduleName parsedModule

                                                    moduleName : ModuleName
                                                    moduleName =
                                                        elmModuleName
                                                            |> List.map Name.fromString
                                                            |> List.drop (List.length model.packageName)
                                                in
                                                if Set.member moduleName model.exposedModules then
                                                    Just ( elmModuleName, parsedModule )

                                                else
                                                    Nothing
                                            )
                                        |> Dict.fromList

                                allParsedModules : Dict ElmModuleName ParsedModule
                                allParsedModules =
                                    model.parsedModules
                                        |> Dict.union latestParsedModulesByName
                            in
                            case model.ir |> IncrementalFrontend.makeModuleDefinitions model.packageName allParsedModules of
                                Ok updatedIR ->
                                    ( { model
                                        | ir =
                                            updatedIR

                                        -- empty out the parsed modules since they were already added to the IR
                                        , parsedModules =
                                            Dict.empty
                                      }
                                    , insertModuleSourcesComplete (encodeVersionedDistribution (IR.toDistribution model.packageName updatedIR))
                                    )

                                Err error ->
                                    ( model
                                    , encodePackageError error
                                        |> reportPackageError
                                    )

                        Err errors ->
                            ( model
                            , errors
                                |> List.concat
                                |> Encode.list encodeParseError
                                |> reportParseError
                            )

                Err error ->
                    ( model
                    , reportError ("Error while decoding module sources: " ++ Decode.errorToString error)
                    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ insertModuleSources InsertModuleSources
        ]


decodeFlags : Decode.Decoder Flags
decodeFlags =
    Decode.map3 Flags
        (Decode.field "packageName"
            (Decode.string
                |> Decode.map Path.fromString
            )
        )
        (Decode.field "exposedModules"
            (Decode.list
                (Decode.string
                    |> Decode.map Path.fromString
                )
                |> Decode.map Set.fromList
            )
        )
        (Decode.field "existingDistribution" (Decode.maybe decodeVersionedDistribution))


decodeModuleSource : Decode.Decoder ModuleSource
decodeModuleSource =
    Decode.map3 ModuleSource
        (Decode.field "path" Decode.string)
        (Decode.field "modifiedAtMillis" (Decode.int |> Decode.map Time.millisToPosix))
        (Decode.field "content" Decode.string)
