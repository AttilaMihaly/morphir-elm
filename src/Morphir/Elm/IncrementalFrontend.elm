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


module Morphir.Elm.IncrementalFrontend exposing
    ( parseModule, makeModuleDefinitions, makeModuleDefinition
    , ParseError, PackageError(..), ModuleError(..), ResolveError(..), ValueError(..)
    )

{-| This module contains the Elm frontend for Morphir. It is used to turn Elm code into Morphir IR. The API is designed
to be "incremental". This means that it's possible to process individual modules without the need to re-process the
whole package.


# Entry points

@docs parseModule, makeModuleDefinitions, makeModuleDefinition


# Errors

@docs ParseError, PackageError, ModuleError, ResolveError, ValueError

-}

import Dict exposing (Dict)
import Elm.Parser
import Elm.Processing as Processing exposing (ProcessContext)
import Elm.Syntax.Declaration exposing (Declaration(..))
import Elm.Syntax.Exposing as Exposing exposing (Exposing)
import Elm.Syntax.Expression as Expression exposing (Expression, Function)
import Elm.Syntax.File exposing (File)
import Elm.Syntax.Module as ElmModule
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern as Pattern exposing (Pattern(..))
import Elm.Syntax.Range exposing (Range)
import Elm.Syntax.Type exposing (ValueConstructor)
import Elm.Syntax.TypeAnnotation exposing (TypeAnnotation(..))
import Graph exposing (Graph)
import Morphir.Compiler as Compiler
import Morphir.Elm.ElmModuleName exposing (ElmModuleName)
import Morphir.Elm.Frontend exposing (defaultDependencies)
import Morphir.Elm.Frontend.Resolve as Resolve exposing (ModuleResolver)
import Morphir.Elm.ParsedModule as ParsedModule exposing (ParsedModule)
import Morphir.Elm.WellKnownOperators as WellKnownOperators
import Morphir.IR as IR exposing (IR)
import Morphir.IR.AccessControlled exposing (AccessControlled, private, public)
import Morphir.IR.Documented exposing (Documented)
import Morphir.IR.FQName as FQName exposing (fQName)
import Morphir.IR.Literal exposing (Literal(..))
import Morphir.IR.Module as Module exposing (ModuleName)
import Morphir.IR.Name as Name exposing (Name)
import Morphir.IR.Package exposing (PackageName)
import Morphir.IR.QName as QName
import Morphir.IR.SDK.Basics as SDKBasics
import Morphir.IR.SDK.List as SDKList
import Morphir.IR.Type as Type exposing (Type)
import Morphir.IR.Type.Rewrite exposing (rewriteType)
import Morphir.IR.Value as Value exposing (Value)
import Morphir.Rewrite as Rewrite
import Morphir.SDK.ResultList as ResultList
import Morphir.Type.Infer as Infer
import Parser
import Set exposing (Set)


{-| Encodes a parse error with location. If there are multiple problems at the same location they will be reported as a
single parse error with multiple problems.
-}
type alias ParseError =
    { location : Compiler.SourceLocation
    , problems : List String
    }


{-| Parses a single module.
-}
parseModule : String -> Result (List ParseError) ParsedModule
parseModule source =
    Elm.Parser.parse source
        |> Result.map ParsedModule.fromRawFile
        |> Result.mapError
            (\deadEnds ->
                deadEnds |> parserDeadEndToParseError
            )


type alias ModuleDef =
    Module.Definition () (Type ())


{-| Package level errors.
-}
type PackageError
    = ModuleDependencyCycles (List (List ElmModuleName))
    | ModuleErrors (List ( ModuleName, ModuleError ))


{-| Module level errors.
-}
type ModuleError
    = MissingDependencies (Set ModuleName)
    | CompilerError Compiler.Error
    | ValueErrors (Dict Name (List ValueError))
    | ResolveErrors (Dict Name (List ResolveError))


type ResolveError
    = ResolveTypeError Compiler.SourceRange Resolve.Error
    | ResolveValueError Compiler.SourceRange Resolve.Error
    | DuplicateNameInPattern Name Compiler.SourceRange Compiler.SourceRange
    | VariableShadowing Name Compiler.SourceRange Compiler.SourceRange


type ValueError
    = OperatorNotSupported Compiler.SourceRange String
    | RecordPatternNotSupported Compiler.SourceRange
    | GLSLExpressionNotSupported Compiler.SourceRange
    | RecursiveDestructuringNotSupported Compiler.SourceRange
    | EmptyApply Compiler.SourceRange
    | EmptyValueName Compiler.SourceRange
    | ValueTypeInferenceError Compiler.SourceRange Infer.TypeError


{-| Turn a list of parsed module sources into Morphir IR. The modules will first be sorted by dependency and then
translated individually. The IR argument and any previously translated modules are used to resolve any dependencies.
-}
makeModuleDefinitions : PackageName -> Dict ElmModuleName ParsedModule -> IR -> Result PackageError IR
makeModuleDefinitions packageName parsedModules ir =
    parsedModules
        |> sortParsedModulesByDependency
        |> Result.andThen
            (\sortedParsedModules ->
                sortedParsedModules
                    |> List.foldl
                        (\( elmModuleName, parsedModule ) irResultSoFar ->
                            let
                                moduleName : ModuleName
                                moduleName =
                                    elmModuleName
                                        |> List.map Name.fromString
                                        |> List.drop (List.length packageName)
                            in
                            irResultSoFar
                                |> Result.andThen
                                    (\irSoFar ->
                                        makeModuleDefinition packageName irSoFar moduleName parsedModule
                                            |> Result.andThen
                                                (\moduleDef ->
                                                    let
                                                        irWithThisModule : IR
                                                        irWithThisModule =
                                                            irSoFar
                                                                |> IR.insertModuleSpecification packageName
                                                                    moduleName
                                                                    (moduleDef
                                                                        |> Module.mapDefinitionAttributes (always ()) identity
                                                                        |> Module.definitionToSpecificationWithPrivate
                                                                    )
                                                    in
                                                    moduleDef
                                                        -- erase source information for types
                                                        |> Module.mapDefinitionAttributes (always ()) identity
                                                        -- infer value types
                                                        |> Infer.inferModuleDefinition irWithThisModule moduleName
                                                        -- map type inference errors to local errors
                                                        |> Result.mapError CompilerError
                                                        -- erase source information for values
                                                        |> Result.map (Module.mapDefinitionAttributes identity (\( _, type_ ) -> type_))
                                                )
                                            |> Result.map (\moduleDef -> IR.insertModuleDefinition packageName moduleName moduleDef irSoFar)
                                            |> Result.mapError (Tuple.pair moduleName)
                                    )
                        )
                        (Ok ir)
                    |> Result.mapError (List.singleton >> ModuleErrors)
            )


{-| Turn a single parsed module source into Morphir IR.
-}
makeModuleDefinition : PackageName -> IR -> ModuleName -> ParsedModule -> Result ModuleError (Module.Definition Compiler.SourceRange Compiler.SourceRange)
makeModuleDefinition packageName ir moduleName parsedModule =
    let
        processedFile : File
        processedFile =
            parsedModule
                |> ParsedModule.toRawFile
                |> Processing.process processContext

        expose : Exposing
        expose =
            processedFile.moduleDefinition
                |> Node.value
                |> ElmModule.exposingList

        types : Dict Name (AccessControlled (Documented (Type.Definition Compiler.SourceRange)))
        types =
            processedFile.declarations
                |> List.filterMap (Node.value >> mapDeclarationToType expose)
                |> Dict.fromList

        valuesResult : Result ModuleError (Dict Name (AccessControlled (Value.Definition Compiler.SourceRange Compiler.SourceRange)))
        valuesResult =
            processedFile.declarations
                |> List.filterMap (mapDeclarationToValue expose)
                |> ResultList.keepAllErrors
                |> Result.mapError (Dict.fromList >> ValueErrors)
                |> Result.map Dict.fromList
    in
    Result.map (Module.Definition types) valuesResult
        |> Result.andThen
            (\moduleDef ->
                let
                    moduleResolver : ModuleResolver
                    moduleResolver =
                        Resolve.createModuleResolver
                            (Resolve.Context
                                defaultDependencies
                                packageName
                                (ir |> IR.moduleSpecifications packageName)
                                (processedFile.imports |> List.map Node.value)
                                moduleName
                                moduleDef
                            )
                in
                resolveLocalNames moduleResolver moduleDef
            )


{-| Maps a declaration in the elm-syntax AST to a Morphir type definition. The return type is optional because
declarations may refer to values or other things that are not types.

The `expose` argument takes the `exposing` clause of the module declaration which is used to add access control to each
type.

The mapping never fails because all elm-syntax type declarations have a Morphir IR mapping.

-}
mapDeclarationToType : Exposing -> Declaration -> Maybe ( Name, AccessControlled (Documented (Type.Definition Compiler.SourceRange)) )
mapDeclarationToType expose declaration =
    let
        extractTypeName : { a | name : Node String } -> Name
        extractTypeName node =
            node.name
                |> Node.value
                |> Name.fromString

        extractTypeParams : { a | generics : List (Node String) } -> List Name
        extractTypeParams node =
            node.generics
                |> List.map (Node.value >> Name.fromString)

        extractDocs : { a | documentation : Maybe (Node String) } -> String
        extractDocs node =
            node.documentation
                |> Maybe.map
                    (Node.value
                        >> String.dropLeft (String.length "{-|")
                        >> String.dropRight (String.length "-}")
                    )
                |> Maybe.withDefault ""
    in
    case declaration of
        AliasDeclaration typeAlias ->
            let
                isExposed : Bool
                isExposed =
                    case expose of
                        Exposing.All _ ->
                            True

                        Exposing.Explicit exposeList ->
                            exposeList
                                |> List.map Node.value
                                |> List.any
                                    (\topLevelExpose ->
                                        case topLevelExpose of
                                            Exposing.TypeOrAliasExpose exposedName ->
                                                exposedName == Node.value typeAlias.name

                                            _ ->
                                                False
                                    )
            in
            Just
                ( extractTypeName typeAlias
                , withAccessControl isExposed
                    (Documented (extractDocs typeAlias)
                        (Type.typeAliasDefinition
                            (extractTypeParams typeAlias)
                            (mapTypeAnnotation typeAlias.typeAnnotation)
                        )
                    )
                )

        CustomTypeDeclaration customType ->
            let
                ( isTypeExposed, areConstructorsExposed ) =
                    case expose of
                        Exposing.All _ ->
                            ( True, True )

                        Exposing.Explicit exposeList ->
                            exposeList
                                |> List.map Node.value
                                |> List.filterMap
                                    (\topLevelExpose ->
                                        case topLevelExpose of
                                            Exposing.TypeOrAliasExpose exposedName ->
                                                if exposedName == Node.value customType.name then
                                                    Just False

                                                else
                                                    Nothing

                                            Exposing.TypeExpose exposedType ->
                                                if exposedType.name == Node.value customType.name then
                                                    case exposedType.open of
                                                        Just _ ->
                                                            Just True

                                                        Nothing ->
                                                            Just False

                                                else
                                                    Nothing

                                            _ ->
                                                Nothing
                                    )
                                |> List.head
                                |> Maybe.map (\isOpen -> ( True, isOpen ))
                                |> Maybe.withDefault ( False, False )

                constructors : Type.Constructors Compiler.SourceRange
                constructors =
                    customType.constructors
                        |> List.map
                            (\constructorNode ->
                                let
                                    constructor : ValueConstructor
                                    constructor =
                                        constructorNode
                                            |> Node.value
                                in
                                ( constructor.name
                                    |> Node.value
                                    |> Name.fromString
                                , constructor.arguments
                                    |> List.indexedMap
                                        (\index arg ->
                                            mapTypeAnnotation arg
                                                |> (\argType ->
                                                        ( [ "arg", String.fromInt (index + 1) ]
                                                        , argType
                                                        )
                                                   )
                                        )
                                )
                            )
                        |> Dict.fromList
            in
            Just
                ( extractTypeName customType
                , withAccessControl isTypeExposed
                    (Documented (extractDocs customType)
                        (Type.customTypeDefinition
                            (extractTypeParams customType)
                            (withAccessControl areConstructorsExposed constructors)
                        )
                    )
                )

        _ ->
            Nothing


{-| Maps a type annotation in the elm-syntax AST to a Morphir type expression.

The mapping never fails because all elm-syntax type annotations have a Morphir IR mapping.

-}
mapTypeAnnotation : Node TypeAnnotation -> Type Compiler.SourceRange
mapTypeAnnotation (Node range typeAnnotation) =
    let
        sourceRange : Compiler.SourceRange
        sourceRange =
            mapRange range
    in
    case typeAnnotation of
        GenericType varName ->
            Type.Variable sourceRange (varName |> Name.fromString)

        Typed (Node _ ( moduleName, localName )) argNodes ->
            Type.Reference sourceRange
                (fQName [] (moduleName |> List.map Name.fromString) (Name.fromString localName))
                (argNodes
                    |> List.map mapTypeAnnotation
                )

        Unit ->
            Type.Unit sourceRange

        Tupled elemNodes ->
            Type.Tuple sourceRange
                (elemNodes
                    |> List.map mapTypeAnnotation
                )

        Record fieldNodes ->
            Type.Record sourceRange
                (fieldNodes
                    |> List.map Node.value
                    |> List.map
                        (\( Node _ fieldName, fieldTypeNode ) ->
                            Type.Field (fieldName |> Name.fromString) (mapTypeAnnotation fieldTypeNode)
                        )
                )

        GenericRecord (Node _ argName) (Node _ fieldNodes) ->
            Type.ExtensibleRecord sourceRange
                (argName |> Name.fromString)
                (fieldNodes
                    |> List.map Node.value
                    |> List.map
                        (\( Node _ fieldName, fieldTypeNode ) ->
                            Type.Field (fieldName |> Name.fromString) (mapTypeAnnotation fieldTypeNode)
                        )
                )

        FunctionTypeAnnotation argTypeNode returnTypeNode ->
            Type.Function sourceRange
                (mapTypeAnnotation argTypeNode)
                (mapTypeAnnotation returnTypeNode)


mapDeclarationToValue : Exposing -> Node Declaration -> Maybe (Result ( Name, List ValueError ) ( Name, AccessControlled (Value.Definition Compiler.SourceRange Compiler.SourceRange) ))
mapDeclarationToValue expose (Node range decl) =
    case decl of
        FunctionDeclaration function ->
            let
                valueName : Name
                valueName =
                    function.declaration
                        |> Node.value
                        |> .name
                        |> Node.value
                        |> Name.fromString

                valueDef : Result (List ValueError) (AccessControlled (Value.Definition Compiler.SourceRange Compiler.SourceRange))
                valueDef =
                    Node range function
                        |> mapFunction
                        |> Result.map public
            in
            valueDef
                |> Result.map (Tuple.pair valueName)
                |> Result.mapError (Tuple.pair valueName)
                |> Just

        _ ->
            Nothing


mapFunction : Node Function -> Result (List ValueError) (Value.Definition Compiler.SourceRange Compiler.SourceRange)
mapFunction (Node functionRange function) =
    let
        expression : Node Expression
        expression =
            Node
                functionRange
                (Expression.LambdaExpression
                    { args =
                        function.declaration
                            |> Node.value
                            |> .arguments
                    , expression =
                        function.declaration
                            |> Node.value
                            |> .expression
                    }
                )
    in
    case function.signature of
        Just (Node _ signature) ->
            mapExpression expression
                |> Result.map (Value.Definition [] (mapTypeAnnotation signature.typeAnnotation))
                |> Result.map liftLambdaArguments

        Nothing ->
            let
                exp =
                    mapExpression expression
            in
            exp
                |> Result.andThen
                    (\body ->
                        Infer.inferValue IR.empty (body |> Value.mapValueAttributes (always ()) identity)
                            |> Result.mapError (\err -> [ ValueTypeInferenceError (mapRange functionRange) err ])
                            |> Result.map (Value.valueAttribute >> Tuple.second >> Type.mapTypeAttributes (always (mapRange functionRange)))
                    )
                |> Result.andThen
                    (\inferredType ->
                        exp
                            |> Result.map (Value.Definition [] inferredType)
                    )
                |> Result.map liftLambdaArguments


{-| Moves lambda arguments into function arguments as much as possible. For example given this function definition:

    foo : Int -> Bool -> ( Int, Int ) -> String
    foo =
        \a ->
            \b ->
                ( c, d ) ->
                    doSomething a b c d

It turns it into the following:

    foo : Int -> Bool -> ( Int, Int ) -> String
    foo a b =
        ( c, d ) ->
            doSomething a b c d

-}
liftLambdaArguments : Value.Definition ta va -> Value.Definition ta va
liftLambdaArguments valueDef =
    case ( valueDef.body, valueDef.outputType ) of
        ( Value.Lambda va (Value.AsPattern _ (Value.WildcardPattern _) argName) lambdaBody, Type.Function _ argType returnType ) ->
            liftLambdaArguments
                { inputTypes = valueDef.inputTypes ++ [ ( argName, va, argType ) ]
                , outputType = returnType
                , body = lambdaBody
                }

        _ ->
            valueDef


mapExpression : Node Expression -> Result (List ValueError) (Value.Value Compiler.SourceRange Compiler.SourceRange)
mapExpression (Node range exp) =
    let
        sourceLocation : Compiler.SourceRange
        sourceLocation =
            mapRange range
    in
    case exp of
        Expression.UnitExpr ->
            Ok (Value.Unit sourceLocation)

        Expression.Application expNodes ->
            let
                toApply : List (Value.Value Compiler.SourceRange Compiler.SourceRange) -> Result (List ValueError) (Value.Value Compiler.SourceRange Compiler.SourceRange)
                toApply valuesReversed =
                    case valuesReversed of
                        [] ->
                            Err [ EmptyApply sourceLocation ]

                        [ singleValue ] ->
                            Ok singleValue

                        lastValue :: restOfValuesReversed ->
                            toApply restOfValuesReversed
                                |> Result.map
                                    (\funValue ->
                                        Value.Apply sourceLocation funValue lastValue
                                    )
            in
            expNodes
                |> List.map mapExpression
                |> ResultList.keepAllErrors
                |> Result.mapError List.concat
                |> Result.andThen (List.reverse >> toApply)

        Expression.OperatorApplication op _ leftNode rightNode ->
            case op of
                "<|" ->
                    -- the purpose of this operator is cleaner syntax so it's not mapped to the IR
                    Result.map2 (Value.Apply sourceLocation)
                        (mapExpression leftNode)
                        (mapExpression rightNode)

                "|>" ->
                    -- the purpose of this operator is cleaner syntax so it's not mapped to the IR
                    Result.map2 (Value.Apply sourceLocation)
                        (mapExpression rightNode)
                        (mapExpression leftNode)

                _ ->
                    Result.map3 (\fun arg1 arg2 -> Value.Apply sourceLocation (Value.Apply sourceLocation fun arg1) arg2)
                        (mapOperator sourceLocation op |> Result.mapError List.singleton)
                        (mapExpression leftNode)
                        (mapExpression rightNode)

        Expression.FunctionOrValue moduleName localName ->
            localName
                |> String.uncons
                |> Result.fromMaybe [ EmptyValueName sourceLocation ]
                |> Result.andThen
                    (\( firstChar, _ ) ->
                        if Char.isUpper firstChar then
                            case ( moduleName, localName ) of
                                ( [], "True" ) ->
                                    Ok (Value.Literal sourceLocation (BoolLiteral True))

                                ( [], "False" ) ->
                                    Ok (Value.Literal sourceLocation (BoolLiteral False))

                                _ ->
                                    Ok (Value.Constructor sourceLocation (fQName [] (moduleName |> List.map Name.fromString) (localName |> Name.fromString)))

                        else
                            Ok (Value.Reference sourceLocation (fQName [] (moduleName |> List.map Name.fromString) (localName |> Name.fromString)))
                    )

        Expression.IfBlock condNode thenNode elseNode ->
            Result.map3 (Value.IfThenElse sourceLocation)
                (mapExpression condNode)
                (mapExpression thenNode)
                (mapExpression elseNode)

        Expression.PrefixOperator op ->
            mapOperator sourceLocation op
                |> Result.mapError List.singleton

        Expression.Operator op ->
            mapOperator sourceLocation op
                |> Result.mapError List.singleton

        Expression.Integer value ->
            Ok (Value.Literal sourceLocation (WholeNumberLiteral value))

        Expression.Hex value ->
            Ok (Value.Literal sourceLocation (WholeNumberLiteral value))

        Expression.Floatable value ->
            Ok (Value.Literal sourceLocation (FloatLiteral value))

        Expression.Negation arg ->
            mapExpression arg
                |> Result.map (SDKBasics.negate sourceLocation sourceLocation)

        Expression.Literal value ->
            Ok (Value.Literal sourceLocation (StringLiteral value))

        Expression.CharLiteral value ->
            Ok (Value.Literal sourceLocation (CharLiteral value))

        Expression.TupledExpression expNodes ->
            expNodes
                |> List.map mapExpression
                |> ResultList.keepAllErrors
                |> Result.mapError List.concat
                |> Result.map (Value.Tuple sourceLocation)

        Expression.ParenthesizedExpression expNode ->
            mapExpression expNode

        Expression.LetExpression letBlock ->
            mapLetExpression sourceLocation letBlock

        Expression.CaseExpression caseBlock ->
            Result.map2 (Value.PatternMatch sourceLocation)
                (mapExpression caseBlock.expression)
                (caseBlock.cases
                    |> List.map
                        (\( patternNode, bodyNode ) ->
                            Result.map2 Tuple.pair
                                (mapPattern patternNode)
                                (mapExpression bodyNode)
                        )
                    |> ResultList.keepAllErrors
                    |> Result.mapError List.concat
                )

        Expression.LambdaExpression lambda ->
            let
                curriedLambda : List (Node Pattern) -> Node Expression -> Result (List ValueError) (Value.Value Compiler.SourceRange Compiler.SourceRange)
                curriedLambda argNodes bodyNode =
                    case argNodes of
                        [] ->
                            mapExpression bodyNode

                        firstArgNode :: restOfArgNodes ->
                            Result.map2 (Value.Lambda sourceLocation)
                                (mapPattern firstArgNode)
                                (curriedLambda restOfArgNodes bodyNode)
            in
            curriedLambda lambda.args lambda.expression

        Expression.RecordExpr fieldNodes ->
            fieldNodes
                |> List.map Node.value
                |> List.map
                    (\( Node _ fieldName, fieldValue ) ->
                        mapExpression fieldValue
                            |> Result.map (Tuple.pair (fieldName |> Name.fromString))
                    )
                |> ResultList.keepAllErrors
                |> Result.mapError List.concat
                |> Result.map (Value.Record sourceLocation)

        Expression.ListExpr itemNodes ->
            itemNodes
                |> List.map mapExpression
                |> ResultList.keepAllErrors
                |> Result.mapError List.concat
                |> Result.map (Value.List sourceLocation)

        Expression.RecordAccess targetNode fieldNameNode ->
            mapExpression targetNode
                |> Result.map
                    (\subjectValue ->
                        Value.Field sourceLocation subjectValue (fieldNameNode |> Node.value |> Name.fromString)
                    )

        Expression.RecordAccessFunction fieldName ->
            Ok (Value.FieldFunction sourceLocation (fieldName |> Name.fromString))

        Expression.RecordUpdateExpression targetVarNameNode fieldNodes ->
            fieldNodes
                |> List.map Node.value
                |> List.map
                    (\( Node _ fieldName, fieldValue ) ->
                        mapExpression fieldValue
                            |> Result.map (Tuple.pair (fieldName |> Name.fromString))
                    )
                |> ResultList.keepAllErrors
                |> Result.mapError List.concat
                |> Result.map
                    (Value.UpdateRecord sourceLocation (targetVarNameNode |> Node.value |> Name.fromString |> Value.Variable sourceLocation))

        Expression.GLSLExpression _ ->
            Err [ GLSLExpressionNotSupported sourceLocation ]


mapLetExpression : Compiler.SourceRange -> Expression.LetBlock -> Result (List ValueError) (Value Compiler.SourceRange Compiler.SourceRange)
mapLetExpression sourceLocation letBlock =
    let
        namesReferredByExpression : Expression -> List String
        namesReferredByExpression expression =
            case expression of
                Expression.Application argNodes ->
                    argNodes |> List.concatMap (Node.value >> namesReferredByExpression)

                Expression.OperatorApplication _ _ (Node _ leftExp) (Node _ rightExp) ->
                    namesReferredByExpression leftExp ++ namesReferredByExpression rightExp

                Expression.FunctionOrValue [] name ->
                    [ name ]

                Expression.IfBlock (Node _ condExp) (Node _ thenExp) (Node _ elseExp) ->
                    namesReferredByExpression condExp ++ namesReferredByExpression thenExp ++ namesReferredByExpression elseExp

                Expression.Negation (Node _ childExp) ->
                    namesReferredByExpression childExp

                Expression.TupledExpression argNodes ->
                    argNodes |> List.concatMap (Node.value >> namesReferredByExpression)

                Expression.ParenthesizedExpression (Node _ childExp) ->
                    namesReferredByExpression childExp

                Expression.LetExpression innerLetBlock ->
                    innerLetBlock.declarations
                        |> List.concatMap
                            (\(Node _ decl) ->
                                case decl of
                                    Expression.LetFunction function ->
                                        function.declaration |> Node.value |> .expression |> Node.value |> namesReferredByExpression

                                    Expression.LetDestructuring _ (Node _ childExp) ->
                                        namesReferredByExpression childExp
                            )
                        |> (++) (innerLetBlock.expression |> Node.value |> namesReferredByExpression)

                Expression.CaseExpression caseBlock ->
                    caseBlock.cases
                        |> List.concatMap
                            (\( _, Node _ childExp ) ->
                                namesReferredByExpression childExp
                            )
                        |> (++) (caseBlock.expression |> Node.value |> namesReferredByExpression)

                Expression.LambdaExpression lambda ->
                    lambda.expression |> Node.value |> namesReferredByExpression

                Expression.RecordExpr setterNodes ->
                    setterNodes |> List.concatMap (\(Node _ ( _, Node _ childExp )) -> namesReferredByExpression childExp)

                Expression.ListExpr argNodes ->
                    argNodes |> List.concatMap (Node.value >> namesReferredByExpression)

                Expression.RecordAccess (Node _ childExp) _ ->
                    namesReferredByExpression childExp

                Expression.RecordUpdateExpression (Node _ recordRef) setterNodes ->
                    recordRef :: (setterNodes |> List.concatMap (\(Node _ ( _, Node _ childExp )) -> namesReferredByExpression childExp))

                _ ->
                    []

        letBlockToValue : List (Node Expression.LetDeclaration) -> Node Expression -> Result (List ValueError) (Value.Value Compiler.SourceRange Compiler.SourceRange)
        letBlockToValue declarationNodes inNode =
            let
                -- build a dictionary from variable name to declaration index
                declarationIndexForName : Dict String Int
                declarationIndexForName =
                    declarationNodes
                        |> List.indexedMap
                            (\index (Node _ decl) ->
                                case decl of
                                    Expression.LetFunction function ->
                                        [ ( function.declaration |> Node.value |> .name |> Node.value, index ) ]

                                    Expression.LetDestructuring (Node _ pattern) _ ->
                                        namesBoundByPattern pattern
                                            |> Set.map (\name -> ( name, index ))
                                            |> Set.toList
                            )
                        |> List.concat
                        |> Dict.fromList

                -- build a dependency graph between declarations
                declarationDependencyGraph : Graph (Node Expression.LetDeclaration) String
                declarationDependencyGraph =
                    let
                        nodes : List (Graph.Node (Node Expression.LetDeclaration))
                        nodes =
                            declarationNodes
                                |> List.indexedMap
                                    (\index declNode ->
                                        Graph.Node index declNode
                                    )

                        edges : List (Graph.Edge String)
                        edges =
                            declarationNodes
                                |> List.indexedMap
                                    (\fromIndex (Node _ decl) ->
                                        case decl of
                                            Expression.LetFunction function ->
                                                function.declaration
                                                    |> Node.value
                                                    |> .expression
                                                    |> Node.value
                                                    |> namesReferredByExpression
                                                    |> List.filterMap
                                                        (\name ->
                                                            declarationIndexForName
                                                                |> Dict.get name
                                                                |> Maybe.map (\toIndex -> Graph.Edge fromIndex toIndex name)
                                                        )

                                            Expression.LetDestructuring _ expression ->
                                                expression
                                                    |> Node.value
                                                    |> namesReferredByExpression
                                                    |> List.filterMap
                                                        (\name ->
                                                            declarationIndexForName
                                                                |> Dict.get name
                                                                |> Maybe.map (\toIndex -> Graph.Edge fromIndex toIndex name)
                                                        )
                                    )
                                |> List.concat
                    in
                    Graph.fromNodesAndEdges nodes edges

                letDeclarationToValue : Node Expression.LetDeclaration -> Result (List ValueError) (Value.Value Compiler.SourceRange Compiler.SourceRange) -> Result (List ValueError) (Value.Value Compiler.SourceRange Compiler.SourceRange)
                letDeclarationToValue letDeclarationNode valueResult =
                    case letDeclarationNode of
                        Node range (Expression.LetFunction function) ->
                            Result.map2 (Value.LetDefinition sourceLocation (function.declaration |> Node.value |> .name |> Node.value |> Name.fromString))
                                (mapFunction (Node range function))
                                valueResult

                        Node range (Expression.LetDestructuring patternNode letExpressionNode) ->
                            Result.map3 (Value.Destructure sourceLocation)
                                (mapPattern patternNode)
                                (mapExpression letExpressionNode)
                                valueResult

                componentGraphToValue : Graph (Node Expression.LetDeclaration) String -> Result (List ValueError) (Value.Value Compiler.SourceRange Compiler.SourceRange) -> Result (List ValueError) (Value.Value Compiler.SourceRange Compiler.SourceRange)
                componentGraphToValue componentGraph valueResult =
                    case componentGraph |> Graph.checkAcyclic of
                        Ok acyclic ->
                            acyclic
                                |> Graph.topologicalSort
                                |> List.foldl
                                    (\nodeContext innerSoFar ->
                                        letDeclarationToValue nodeContext.node.label innerSoFar
                                    )
                                    valueResult

                        Err _ ->
                            Result.map2 (Value.LetRecursion sourceLocation)
                                (componentGraph
                                    |> Graph.nodes
                                    |> List.map
                                        (\graphNode ->
                                            case graphNode.label of
                                                Node range (Expression.LetFunction function) ->
                                                    mapFunction (Node range function)
                                                        |> Result.map (Tuple.pair (function.declaration |> Node.value |> .name |> Node.value |> Name.fromString))

                                                Node range (Expression.LetDestructuring _ _) ->
                                                    Err [ RecursiveDestructuringNotSupported sourceLocation ]
                                        )
                                    |> ResultList.keepAllErrors
                                    |> Result.mapError List.concat
                                    |> Result.map Dict.fromList
                                )
                                valueResult
            in
            case declarationDependencyGraph |> Graph.stronglyConnectedComponents of
                Ok acyclic ->
                    acyclic
                        |> Graph.topologicalSort
                        |> List.foldl
                            (\nodeContext soFar ->
                                letDeclarationToValue nodeContext.node.label soFar
                            )
                            (mapExpression inNode)

                Err components ->
                    components
                        |> List.foldl
                            componentGraphToValue
                            (mapExpression inNode)
    in
    letBlockToValue letBlock.declarations letBlock.expression


{-| Maps an elm-syntax pattern to a Morphir IR pattern. Certain patterns are not supported so the function
may return an error.
-}
mapPattern : Node Pattern -> Result (List ValueError) (Value.Pattern Compiler.SourceRange)
mapPattern (Node range pattern) =
    let
        sourceRange : Compiler.SourceRange
        sourceRange =
            mapRange range
    in
    case pattern of
        Pattern.AllPattern ->
            Ok (Value.WildcardPattern sourceRange)

        Pattern.UnitPattern ->
            Ok (Value.UnitPattern sourceRange)

        Pattern.CharPattern char ->
            Ok (Value.LiteralPattern sourceRange (CharLiteral char))

        Pattern.StringPattern string ->
            Ok (Value.LiteralPattern sourceRange (StringLiteral string))

        Pattern.IntPattern int ->
            Ok (Value.LiteralPattern sourceRange (WholeNumberLiteral int))

        Pattern.HexPattern int ->
            Ok (Value.LiteralPattern sourceRange (WholeNumberLiteral int))

        Pattern.FloatPattern float ->
            Ok (Value.LiteralPattern sourceRange (FloatLiteral float))

        Pattern.TuplePattern elemNodes ->
            elemNodes
                |> List.map mapPattern
                |> ResultList.keepAllErrors
                |> Result.mapError List.concat
                |> Result.map (Value.TuplePattern sourceRange)

        Pattern.RecordPattern _ ->
            Err [ RecordPatternNotSupported sourceRange ]

        Pattern.UnConsPattern headNode tailNode ->
            Result.map2 (Value.HeadTailPattern sourceRange)
                (mapPattern headNode)
                (mapPattern tailNode)

        Pattern.ListPattern itemNodes ->
            let
                toPattern : List (Node Pattern) -> Result (List ValueError) (Value.Pattern Compiler.SourceRange)
                toPattern patternNodes =
                    case patternNodes of
                        [] ->
                            Ok (Value.EmptyListPattern sourceRange)

                        headNode :: tailNodes ->
                            Result.map2 (Value.HeadTailPattern sourceRange)
                                (mapPattern headNode)
                                (toPattern tailNodes)
            in
            toPattern itemNodes

        Pattern.VarPattern name ->
            Ok (Value.AsPattern sourceRange (Value.WildcardPattern sourceRange) (Name.fromString name))

        Pattern.NamedPattern qualifiedNameRef argNodes ->
            let
                qualifiedName =
                    qualifiedNameRef.name
                        |> Name.fromString
                        |> QName.fromName (qualifiedNameRef.moduleName |> List.map Name.fromString)
                        |> FQName.fromQName []
            in
            case ( qualifiedNameRef.moduleName, qualifiedNameRef.name ) of
                ( [], "True" ) ->
                    Ok (Value.LiteralPattern sourceRange (BoolLiteral True))

                ( [], "False" ) ->
                    Ok (Value.LiteralPattern sourceRange (BoolLiteral False))

                _ ->
                    argNodes
                        |> List.map mapPattern
                        |> ResultList.keepAllErrors
                        |> Result.mapError List.concat
                        |> Result.map (Value.ConstructorPattern sourceRange qualifiedName)

        Pattern.AsPattern subjectNode aliasNode ->
            mapPattern subjectNode
                |> Result.map (\subject -> Value.AsPattern sourceRange subject (aliasNode |> Node.value |> Name.fromString))

        Pattern.ParenthesizedPattern childNode ->
            mapPattern childNode


namesBoundByPattern : Pattern -> Set String
namesBoundByPattern p =
    let
        namesBound : Pattern -> List String
        namesBound pattern =
            case pattern of
                TuplePattern elemPatternNodes ->
                    elemPatternNodes |> List.concatMap (Node.value >> namesBound)

                RecordPattern fieldNameNodes ->
                    fieldNameNodes |> List.map Node.value

                UnConsPattern (Node _ headPattern) (Node _ tailPattern) ->
                    namesBound headPattern ++ namesBound tailPattern

                ListPattern itemPatternNodes ->
                    itemPatternNodes |> List.concatMap (Node.value >> namesBound)

                VarPattern name ->
                    [ name ]

                NamedPattern _ argPatternNodes ->
                    argPatternNodes |> List.concatMap (Node.value >> namesBound)

                AsPattern (Node _ childPattern) (Node _ alias) ->
                    alias :: namesBound childPattern

                ParenthesizedPattern (Node _ childPattern) ->
                    namesBound childPattern

                _ ->
                    []
    in
    namesBound p
        |> Set.fromList


{-| Maps an elm-syntax operator to a Morphir IR value expression. Certain operators are not supported so the function
may return an error.
-}
mapOperator : Compiler.SourceRange -> String -> Result ValueError (Value.Value Compiler.SourceRange Compiler.SourceRange)
mapOperator sourceRange op =
    case op of
        "||" ->
            Ok <| SDKBasics.or sourceRange

        "&&" ->
            Ok <| SDKBasics.and sourceRange

        "==" ->
            Ok <| SDKBasics.equal sourceRange

        "/=" ->
            Ok <| SDKBasics.notEqual sourceRange

        "<" ->
            Ok <| SDKBasics.lessThan sourceRange

        ">" ->
            Ok <| SDKBasics.greaterThan sourceRange

        "<=" ->
            Ok <| SDKBasics.lessThanOrEqual sourceRange

        ">=" ->
            Ok <| SDKBasics.greaterThanOrEqual sourceRange

        "++" ->
            Err (OperatorNotSupported sourceRange "The ++ operator is currently not supported. Please use String.append or List.append. See docs/error-append-not-supported.md")

        "+" ->
            Ok <| SDKBasics.add sourceRange

        "-" ->
            Ok <| SDKBasics.subtract sourceRange

        "*" ->
            Ok <| SDKBasics.multiply sourceRange

        "/" ->
            Ok <| SDKBasics.divide sourceRange

        "//" ->
            Ok <| SDKBasics.integerDivide sourceRange

        "^" ->
            Ok <| SDKBasics.power sourceRange

        "<<" ->
            Ok <| SDKBasics.composeLeft sourceRange

        ">>" ->
            Ok <| SDKBasics.composeRight sourceRange

        "::" ->
            Ok <| SDKList.construct sourceRange

        _ ->
            Err (OperatorNotSupported sourceRange <| "OperatorApplication: " ++ op)


resolveLocalNames : ModuleResolver -> Module.Definition Compiler.SourceRange Compiler.SourceRange -> Result ModuleError (Module.Definition Compiler.SourceRange Compiler.SourceRange)
resolveLocalNames moduleResolver moduleDef =
    let
        rewriteValues : Dict Name Compiler.SourceRange -> Value Compiler.SourceRange Compiler.SourceRange -> Result (List ResolveError) (Value Compiler.SourceRange Compiler.SourceRange)
        rewriteValues variables value =
            resolveVariablesAndReferences variables moduleResolver value

        typesResult : Result ModuleError (Dict Name (AccessControlled (Documented (Type.Definition Compiler.SourceRange))))
        typesResult =
            moduleDef.types
                |> Dict.toList
                |> List.map
                    (\( typeName, typeDef ) ->
                        typeDef.value.value
                            |> Type.mapDefinition (rewriteTypes moduleResolver)
                            |> Result.map (Documented typeDef.value.doc)
                            |> Result.map (AccessControlled typeDef.access)
                            |> Result.map (Tuple.pair typeName)
                            |> Result.mapError (List.concat >> Tuple.pair typeName)
                    )
                |> ResultList.keepAllErrors
                |> Result.map Dict.fromList
                |> Result.mapError (Dict.fromList >> ResolveErrors)

        valuesResult : Result ModuleError (Dict Name (AccessControlled (Value.Definition Compiler.SourceRange Compiler.SourceRange)))
        valuesResult =
            moduleDef.values
                |> Dict.toList
                |> List.map
                    (\( valueName, valueDef ) ->
                        let
                            variables : Dict Name Compiler.SourceRange
                            variables =
                                valueDef.value.inputTypes
                                    |> List.map (\( name, loc, _ ) -> ( name, loc ))
                                    |> Dict.fromList
                        in
                        valueDef.value
                            |> Value.mapDefinition
                                (rewriteTypes moduleResolver)
                                (rewriteValues variables)
                            |> Result.map (AccessControlled valueDef.access)
                            |> Result.map (Tuple.pair valueName)
                            |> Result.mapError (List.concat >> Tuple.pair valueName)
                    )
                |> ResultList.keepAllErrors
                |> Result.map Dict.fromList
                |> Result.mapError (Dict.fromList >> ResolveErrors)
    in
    Result.map2 Module.Definition
        typesResult
        valuesResult


rewriteTypes : ModuleResolver -> Type Compiler.SourceRange -> Result (List ResolveError) (Type Compiler.SourceRange)
rewriteTypes moduleResolver =
    Rewrite.bottomUp rewriteType
        (\tpe ->
            case tpe of
                Type.Reference sourceLocation refFullName args ->
                    moduleResolver.resolveType
                        (refFullName |> FQName.getModulePath |> List.map Name.toTitleCase)
                        (refFullName |> FQName.getLocalName |> Name.toTitleCase)
                        |> Result.map
                            (\resolvedFullName ->
                                Type.Reference sourceLocation resolvedFullName args
                            )
                        |> Result.mapError (ResolveTypeError sourceLocation >> List.singleton)
                        |> Just

                _ ->
                    Nothing
        )


resolveVariablesAndReferences : Dict Name Compiler.SourceRange -> ModuleResolver -> Value Compiler.SourceRange Compiler.SourceRange -> Result (List ResolveError) (Value Compiler.SourceRange Compiler.SourceRange)
resolveVariablesAndReferences variables moduleResolver value =
    let
        unionNames : (Name -> Compiler.SourceRange -> Compiler.SourceRange -> ResolveError) -> Dict Name Compiler.SourceRange -> Dict Name Compiler.SourceRange -> Result (List ResolveError) (Dict Name Compiler.SourceRange)
        unionNames toError namesA namesB =
            let
                duplicateNames : List Name
                duplicateNames =
                    Set.intersect (namesA |> Dict.keys |> Set.fromList) (namesB |> Dict.keys |> Set.fromList)
                        |> Set.toList
            in
            if List.isEmpty duplicateNames then
                Ok (Dict.union namesA namesB)

            else
                Err
                    (duplicateNames
                        |> List.filterMap
                            (\name ->
                                Maybe.map2 (toError name)
                                    (namesA |> Dict.get name)
                                    (namesB |> Dict.get name)
                            )
                    )

        unionPatternNames : Dict Name Compiler.SourceRange -> Dict Name Compiler.SourceRange -> Result (List ResolveError) (Dict Name Compiler.SourceRange)
        unionPatternNames =
            unionNames DuplicateNameInPattern

        unionVariableNames : Dict Name Compiler.SourceRange -> Dict Name Compiler.SourceRange -> Result (List ResolveError) (Dict Name Compiler.SourceRange)
        unionVariableNames =
            unionNames VariableShadowing

        namesBoundInPattern : Value.Pattern Compiler.SourceRange -> Result (List ResolveError) (Dict Name Compiler.SourceRange)
        namesBoundInPattern pattern =
            case pattern of
                Value.AsPattern sourceLocation subjectPattern alias ->
                    namesBoundInPattern subjectPattern
                        |> Result.andThen
                            (\subjectNames ->
                                unionPatternNames subjectNames
                                    (Dict.singleton alias sourceLocation)
                            )

                Value.TuplePattern _ elems ->
                    elems
                        |> List.map namesBoundInPattern
                        |> List.foldl
                            (\nextNames soFar ->
                                soFar
                                    |> Result.andThen
                                        (\namesSoFar ->
                                            nextNames
                                                |> Result.andThen (unionPatternNames namesSoFar)
                                        )
                            )
                            (Ok Dict.empty)

                Value.ConstructorPattern _ _ args ->
                    args
                        |> List.map namesBoundInPattern
                        |> List.foldl
                            (\nextNames soFar ->
                                soFar
                                    |> Result.andThen
                                        (\namesSoFar ->
                                            nextNames
                                                |> Result.andThen (unionPatternNames namesSoFar)
                                        )
                            )
                            (Ok Dict.empty)

                Value.HeadTailPattern _ headPattern tailPattern ->
                    namesBoundInPattern headPattern
                        |> Result.andThen
                            (\headNames ->
                                namesBoundInPattern tailPattern
                                    |> Result.andThen (unionPatternNames headNames)
                            )

                _ ->
                    Ok Dict.empty

        resolveValueDefinition def variablesDefNamesAndArgs =
            Result.map3 Value.Definition
                (def.inputTypes
                    |> List.map
                        (\( argName, a, argType ) ->
                            rewriteTypes moduleResolver argType
                                |> Result.map (\t -> ( argName, a, t ))
                        )
                    |> ResultList.keepAllErrors
                    |> Result.mapError List.concat
                )
                (rewriteTypes moduleResolver def.outputType)
                (resolveVariablesAndReferences variablesDefNamesAndArgs moduleResolver def.body)
    in
    case value of
        Value.Constructor sourceLocation ( [], modulePath, localName ) ->
            moduleResolver.resolveCtor
                (modulePath |> List.map Name.toTitleCase)
                (localName |> Name.toTitleCase)
                |> Result.map
                    (\resolvedFullName ->
                        Value.Constructor sourceLocation resolvedFullName
                    )
                |> Result.mapError (ResolveValueError sourceLocation >> List.singleton)

        Value.Reference sourceLocation ( [], modulePath, localName ) ->
            if List.isEmpty modulePath && (variables |> Dict.member localName) then
                Ok (Value.Variable sourceLocation localName)

            else
                moduleResolver.resolveValue
                    (modulePath |> List.map Name.toTitleCase)
                    (localName |> Name.toCamelCase)
                    |> Result.map
                        (\resolvedFullName ->
                            Value.Reference sourceLocation resolvedFullName
                        )
                    |> Result.mapError (ResolveValueError sourceLocation >> List.singleton)

        Value.Lambda a argPattern bodyValue ->
            Result.map2 (Value.Lambda a)
                (resolvePatternReferences moduleResolver argPattern)
                (namesBoundInPattern argPattern
                    |> Result.andThen
                        (\patternNames ->
                            unionVariableNames variables patternNames
                        )
                    |> Result.andThen
                        (\newVariables ->
                            resolveVariablesAndReferences newVariables moduleResolver bodyValue
                        )
                )

        Value.LetDefinition sourceLocation name def inValue ->
            Result.map2 (Value.LetDefinition sourceLocation name)
                (def.inputTypes
                    |> List.map (\( argName, loc, _ ) -> ( argName, loc ))
                    |> Dict.fromList
                    |> Dict.insert name sourceLocation
                    |> unionVariableNames variables
                    |> Result.andThen (resolveValueDefinition def)
                )
                (unionVariableNames variables (Dict.singleton name sourceLocation)
                    |> Result.andThen
                        (\newVariables ->
                            resolveVariablesAndReferences newVariables moduleResolver inValue
                        )
                )

        Value.LetRecursion sourceLocation defs inValue ->
            defs
                |> Dict.map (\_ _ -> sourceLocation)
                |> unionVariableNames variables
                |> Result.andThen
                    (\variablesAndDefNames ->
                        Result.map2 (Value.LetRecursion sourceLocation)
                            (defs
                                |> Dict.toList
                                |> List.map
                                    (\( name, def ) ->
                                        def.inputTypes
                                            |> List.map (\( argName, loc, _ ) -> ( argName, loc ))
                                            |> Dict.fromList
                                            |> unionVariableNames variablesAndDefNames
                                            |> Result.andThen
                                                (\variablesDefNamesAndArgs ->
                                                    Result.map (Tuple.pair name)
                                                        (resolveValueDefinition def variablesDefNamesAndArgs)
                                                )
                                    )
                                |> ResultList.keepAllErrors
                                |> Result.mapError List.concat
                                |> Result.map Dict.fromList
                            )
                            (resolveVariablesAndReferences variablesAndDefNames moduleResolver inValue)
                    )

        Value.Destructure a pattern subjectValue inValue ->
            Result.map3 (Value.Destructure a)
                (resolvePatternReferences moduleResolver pattern)
                (resolveVariablesAndReferences variables moduleResolver subjectValue)
                (namesBoundInPattern pattern
                    |> Result.andThen
                        (\patternNames ->
                            unionVariableNames variables patternNames
                        )
                    |> Result.andThen
                        (\newVariables ->
                            resolveVariablesAndReferences newVariables moduleResolver inValue
                        )
                )

        Value.PatternMatch a matchValue cases ->
            Result.map2 (Value.PatternMatch a)
                (resolveVariablesAndReferences variables moduleResolver matchValue)
                (cases
                    |> List.map
                        (\( casePattern, caseValue ) ->
                            Result.map2 Tuple.pair
                                (resolvePatternReferences moduleResolver casePattern)
                                (namesBoundInPattern casePattern
                                    |> Result.andThen
                                        (\patternNames ->
                                            unionVariableNames variables patternNames
                                        )
                                    |> Result.andThen
                                        (\newVariables ->
                                            resolveVariablesAndReferences newVariables moduleResolver caseValue
                                        )
                                )
                        )
                    |> ResultList.keepAllErrors
                    |> Result.mapError List.concat
                )

        Value.Tuple a elems ->
            elems
                |> List.map (resolveVariablesAndReferences variables moduleResolver)
                |> ResultList.keepAllErrors
                |> Result.mapError List.concat
                |> Result.map (Value.Tuple a)

        Value.List a items ->
            items
                |> List.map (resolveVariablesAndReferences variables moduleResolver)
                |> ResultList.keepAllErrors
                |> Result.mapError List.concat
                |> Result.map (Value.List a)

        Value.Record a fields ->
            fields
                |> List.map
                    (\( fieldName, fieldValue ) ->
                        resolveVariablesAndReferences variables moduleResolver fieldValue
                            |> Result.map (Tuple.pair fieldName)
                    )
                |> ResultList.keepAllErrors
                |> Result.mapError List.concat
                |> Result.map (Value.Record a)

        Value.Field a subjectValue fieldName ->
            resolveVariablesAndReferences variables moduleResolver subjectValue
                |> Result.map (\s -> Value.Field a s fieldName)

        Value.Apply a funValue argValue ->
            Result.map2 (Value.Apply a)
                (resolveVariablesAndReferences variables moduleResolver funValue)
                (resolveVariablesAndReferences variables moduleResolver argValue)

        Value.IfThenElse a condValue thenValue elseValue ->
            Result.map3 (Value.IfThenElse a)
                (resolveVariablesAndReferences variables moduleResolver condValue)
                (resolveVariablesAndReferences variables moduleResolver thenValue)
                (resolveVariablesAndReferences variables moduleResolver elseValue)

        Value.UpdateRecord a subjectValue newFieldValues ->
            Result.map2 (Value.UpdateRecord a)
                (resolveVariablesAndReferences variables moduleResolver subjectValue)
                (newFieldValues
                    |> List.map
                        (\( fieldName, fieldValue ) ->
                            resolveVariablesAndReferences variables moduleResolver fieldValue
                                |> Result.map (Tuple.pair fieldName)
                        )
                    |> ResultList.keepAllErrors
                    |> Result.mapError List.concat
                )

        _ ->
            Ok value


{-| Resolve references with local names into fully-qualified names. Currently this will only apply to
constructor patterns as they are the only ones with a reference to other names.
-}
resolvePatternReferences : ModuleResolver -> Value.Pattern Compiler.SourceRange -> Result (List ResolveError) (Value.Pattern Compiler.SourceRange)
resolvePatternReferences moduleResolver pattern =
    case pattern of
        Value.AsPattern sourceLocation subjectPattern alias ->
            Result.map (\resolvedSubjectPattern -> Value.AsPattern sourceLocation resolvedSubjectPattern alias)
                (resolvePatternReferences moduleResolver subjectPattern)

        Value.TuplePattern sourceLocation elems ->
            Result.map (\resolvedElems -> Value.TuplePattern sourceLocation resolvedElems)
                (elems
                    |> List.map (resolvePatternReferences moduleResolver)
                    |> ResultList.keepAllErrors
                    |> Result.mapError List.concat
                )

        Value.ConstructorPattern sourceLocation ( [], modulePath, localName ) argPatterns ->
            Result.map2
                (\resolvedFullName resolvedArgPatterns ->
                    Value.ConstructorPattern sourceLocation resolvedFullName resolvedArgPatterns
                )
                (moduleResolver.resolveCtor
                    (modulePath |> List.map Name.toTitleCase)
                    (localName |> Name.toTitleCase)
                    |> Result.mapError (ResolveValueError sourceLocation >> List.singleton)
                )
                (argPatterns
                    |> List.map (resolvePatternReferences moduleResolver)
                    |> ResultList.keepAllErrors
                    |> Result.mapError List.concat
                )

        Value.HeadTailPattern sourceLocation headPattern tailPattern ->
            Result.map2 (Value.HeadTailPattern sourceLocation)
                (resolvePatternReferences moduleResolver headPattern)
                (resolvePatternReferences moduleResolver tailPattern)

        _ ->
            Ok pattern


{-| Function that turns a list of parser errors into application specific errors. It also groups the errors by their
position so that multiple errors reported on the same position become a single error with multiple problems.
-}
parserDeadEndToParseError : List Parser.DeadEnd -> List ParseError
parserDeadEndToParseError deadEnds =
    deadEnds
        |> List.foldl
            (\deadEnd dict ->
                dict
                    |> Dict.update ( deadEnd.row, deadEnd.col )
                        (\maybeProblemsSoFar ->
                            case maybeProblemsSoFar of
                                Just problemsSoFar ->
                                    Just (deadEnd.problem :: problemsSoFar)

                                Nothing ->
                                    Just [ deadEnd.problem ]
                        )
            )
            Dict.empty
        |> Dict.toList
        |> List.map
            (\( ( row, col ), problems ) ->
                { problems =
                    problems
                        |> List.map mapParserProblem
                , location =
                    { row = row
                    , column = col
                    }
                }
            )


{-| Utility function to map a parser problem enumeration into a string explanation.
-}
mapParserProblem : Parser.Problem -> String
mapParserProblem problem =
    case problem of
        Parser.Expecting something ->
            "Expecting '" ++ something ++ "'"

        Parser.ExpectingInt ->
            "Expecting integer"

        Parser.ExpectingHex ->
            "Expecting hexadecimal"

        Parser.ExpectingOctal ->
            "Expecting octal"

        Parser.ExpectingBinary ->
            "Expecting binary"

        Parser.ExpectingFloat ->
            "Expecting float"

        Parser.ExpectingNumber ->
            "Expecting number"

        Parser.ExpectingVariable ->
            "Expecting variable"

        Parser.ExpectingSymbol symbol ->
            "Expecting symbol: " ++ symbol

        Parser.ExpectingKeyword keyword ->
            "Expecting keyword: " ++ keyword

        Parser.ExpectingEnd ->
            "Expecting end"

        Parser.UnexpectedChar ->
            "Unexpected character"

        Parser.Problem message ->
            "Problem: " ++ message

        Parser.BadRepeat ->
            "Bad repeat"


{-| Sorts parsed modules in dependency order.
-}
sortParsedModulesByDependency : Dict ElmModuleName ParsedModule -> Result PackageError (List ( ElmModuleName, ParsedModule ))
sortParsedModulesByDependency parsedModules =
    parsedModulesToDependencyGraph (Dict.values parsedModules)
        |> Graph.stronglyConnectedComponents
        |> Result.map
            (\acyclicGraph ->
                acyclicGraph
                    |> Graph.topologicalSort
                    |> List.filterMap
                        (\nodeContext ->
                            parsedModules
                                |> Dict.get nodeContext.node.label
                                |> Maybe.map (Tuple.pair nodeContext.node.label)
                        )
                    |> List.reverse
            )
        |> Result.mapError
            (\components ->
                ModuleDependencyCycles
                    (components
                        |> List.filterMap
                            (\component ->
                                case Graph.checkAcyclic component of
                                    Err _ ->
                                        Just component

                                    Ok _ ->
                                        Nothing
                            )
                        |> List.map
                            (\cyclicGraph ->
                                cyclicGraph
                                    |> Graph.nodes
                                    |> List.map .label
                            )
                    )
            )


{-| Build a dependency graph of parsed modules.
-}
parsedModulesToDependencyGraph : List ParsedModule -> Graph ElmModuleName ()
parsedModulesToDependencyGraph parsedModules =
    let
        moduleNameToIndex : Dict ElmModuleName Int
        moduleNameToIndex =
            parsedModules
                |> List.indexedMap
                    (\index rawFile ->
                        ( ParsedModule.moduleName rawFile, index )
                    )
                |> Dict.fromList
    in
    Graph.fromNodeLabelsAndEdgePairs (parsedModules |> List.map ParsedModule.moduleName)
        (parsedModules
            |> List.indexedMap
                (\index parsedModule ->
                    parsedModule
                        |> ParsedModule.importedModules
                        |> List.filterMap
                            (\importedModule ->
                                Dict.get importedModule moduleNameToIndex
                            )
                        |> List.map (Tuple.pair index)
                )
            |> List.concat
        )


{-| ProcessContext is used by the elm-syntax library to make sure infix operators declared within the package are
properly parsed. Since we don't allow infix operator declarations in the model we can eliminate this complexity and use
a single static ProcessContext.
-}
processContext : ProcessContext
processContext =
    let
        withWellKnownOperators : ProcessContext -> ProcessContext
        withWellKnownOperators context =
            List.foldl Processing.addDependency context WellKnownOperators.wellKnownOperators
    in
    Processing.init
        |> withWellKnownOperators


mapRange : Range -> Compiler.SourceRange
mapRange range =
    { start = range.start
    , end = range.end
    }


withAccessControl : Bool -> a -> AccessControlled a
withAccessControl isExposed a =
    if isExposed then
        public a

    else
        private a
