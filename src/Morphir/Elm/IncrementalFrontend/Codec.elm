module Morphir.Elm.IncrementalFrontend.Codec exposing (..)

import Dict
import Json.Encode as Encode
import Morphir.Compiler.Codec as CompilerCodec exposing (encodeSourceLocation, encodeSourceRange)
import Morphir.Elm.Frontend.Resolve as Resolve
import Morphir.Elm.IncrementalFrontend exposing (ModuleError(..), PackageError(..), ParseError, ResolveError(..), ValueError(..))
import Morphir.IR.Name.Codec exposing (encodeName)
import Morphir.IR.Path.Codec exposing (encodePath)
import Morphir.Type.Infer.Codec as InferCodec
import Set


encodeParseError : ParseError -> Encode.Value
encodeParseError parseError =
    Encode.object
        [ ( "location", encodeSourceLocation parseError.location )
        , ( "problems", Encode.list Encode.string parseError.problems )
        ]


encodePackageError : PackageError -> Encode.Value
encodePackageError packageError =
    case packageError of
        ModuleDependencyCycles cycles ->
            Encode.list identity
                [ Encode.string "ModuleDependencyCycles"
                , Encode.list (Encode.list (Encode.list Encode.string)) cycles
                ]

        ModuleErrors moduleErrors ->
            Encode.list identity
                [ Encode.string "ModuleErrors"
                , moduleErrors
                    |> List.map
                        (\( moduleName, moduleError ) ->
                            Encode.list identity
                                [ encodePath moduleName
                                , encodeModuleError moduleError
                                ]
                        )
                    |> Encode.list identity
                ]


encodeModuleError : ModuleError -> Encode.Value
encodeModuleError moduleError =
    case moduleError of
        MissingDependencies moduleNames ->
            Encode.list identity
                [ Encode.string "MissingDependencies"
                , Encode.list encodePath (Set.toList moduleNames)
                ]

        CompilerError compilerError ->
            Encode.list identity
                [ Encode.string "CompilerError"
                , CompilerCodec.encodeError compilerError
                ]

        ValueErrors valueErrors ->
            Encode.list identity
                [ Encode.string "ValueErrors"
                , valueErrors
                    |> Dict.toList
                    |> Encode.list
                        (\( localName, errors ) ->
                            Encode.list identity
                                [ encodeName localName
                                , Encode.list encodeValueError errors
                                ]
                        )
                ]

        ResolveErrors resolveErrors ->
            Encode.list identity
                [ Encode.string "ResolveErrors"
                , resolveErrors
                    |> Dict.toList
                    |> Encode.list
                        (\( localName, errors ) ->
                            Encode.list identity
                                [ encodeName localName
                                , Encode.list encodeResolveError errors
                                ]
                        )
                ]


encodeResolveError : ResolveError -> Encode.Value
encodeResolveError resolveError =
    case resolveError of
        ResolveTypeError sourceRange error ->
            Encode.list identity
                [ Encode.string "ResolveTypeError"
                , encodeSourceRange sourceRange
                , Resolve.encodeError error
                ]

        ResolveValueError sourceRange error ->
            Encode.list identity
                [ Encode.string "ResolveValueError"
                , encodeSourceRange sourceRange
                , Resolve.encodeError error
                ]

        DuplicateNameInPattern name sourceRange1 sourceRange2 ->
            Encode.list identity
                [ Encode.string "DuplicateNameInPattern"
                , encodeName name
                , encodeSourceRange sourceRange1
                , encodeSourceRange sourceRange2
                ]

        VariableShadowing name sourceRange1 sourceRange2 ->
            Encode.list identity
                [ Encode.string "VariableShadowing"
                , encodeName name
                , encodeSourceRange sourceRange1
                , encodeSourceRange sourceRange2
                ]


encodeValueError : ValueError -> Encode.Value
encodeValueError valueError =
    case valueError of
        OperatorNotSupported sourceRange message ->
            Encode.list identity
                [ Encode.string "OperatorNotSupported"
                , encodeSourceRange sourceRange
                , Encode.string message
                ]

        RecordPatternNotSupported sourceRange ->
            Encode.list identity
                [ Encode.string "RecordPatternNotSupported"
                , encodeSourceRange sourceRange
                ]

        GLSLExpressionNotSupported sourceRange ->
            Encode.list identity
                [ Encode.string "GLSLExpressionNotSupported"
                , encodeSourceRange sourceRange
                ]

        RecursiveDestructuringNotSupported sourceRange ->
            Encode.list identity
                [ Encode.string "RecursiveDestructuringNotSupported"
                , encodeSourceRange sourceRange
                ]

        EmptyApply sourceRange ->
            Encode.list identity
                [ Encode.string "EmptyApply"
                , encodeSourceRange sourceRange
                ]

        EmptyValueName sourceRange ->
            Encode.list identity
                [ Encode.string "EmptyValueName"
                , encodeSourceRange sourceRange
                ]

        ValueTypeInferenceError sourceRange typeError ->
            Encode.list identity
                [ Encode.string "ValueTypeInferenceError"
                , encodeSourceRange sourceRange
                , InferCodec.encodeTypeError typeError
                ]
