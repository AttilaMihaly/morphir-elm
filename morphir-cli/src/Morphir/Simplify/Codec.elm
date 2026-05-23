module Morphir.Simplify.Codec exposing (encodeModuleDefinition)

{-| Simplified JSON encoding for Morphir IR.

Encoding conventions:

  - `Name` → TitleCase string, e.g. `"FooBar"`
  - `FQName` → `"Package.Path:Module.Path:localName"` string
  - Tagged unions with fields → `{"Tag": {"field1": ..., ...}}`
  - Tagged unions with no fields → bare string `"Tag"`
  - Dicts keyed by Name → JSON objects with TitleCase field names
  - List-valued fields are omitted entirely when the list is empty
  - `doc` fields are omitted when the string is empty
  - Access control is dropped — everything is assumed public
  - The `Documented` wrapper is dissolved; `doc` is merged into the value object
  - `Value.Literal` nodes are flattened to the specific literal variant tag

-}

import Dict
import Json.Encode as Encode
import Morphir.IR.FQName as FQName exposing (FQName)
import Morphir.IR.Literal exposing (Literal(..))
import Morphir.IR.Module as Module
import Morphir.IR.Name as Name exposing (Name)
import Morphir.IR.Type as Type exposing (Constructors, Type)
import Morphir.IR.Value as Value exposing (Pattern, Value)
import Morphir.SDK.Decimal as Decimal


encodeName : Name -> Encode.Value
encodeName name =
    Encode.string (Name.toTitleCase name)


encodeFQName : FQName -> Encode.Value
encodeFQName fqn =
    Encode.string (FQName.toString fqn)


{-| Encode a tagged union variant.
Zero fields → bare string tag. One or more fields → `{"Tag": {"field": ...}}`.
-}
encodeTaggedUnion : String -> List ( String, Encode.Value ) -> Encode.Value
encodeTaggedUnion tag fields =
    if List.isEmpty fields then
        Encode.string tag

    else
        Encode.object [ ( tag, Encode.object fields ) ]


{-| Encode a tagged union variant whose entire payload is a single value, not a named-field object.
Produces `{"Tag": value}`.
-}
encodeTaggedValue : String -> Encode.Value -> Encode.Value
encodeTaggedValue tag value =
    Encode.object [ ( tag, value ) ]


{-| Produce a single-element field list for a non-empty list, or [] to omit the field. -}
listField : String -> List a -> (a -> Encode.Value) -> List ( String, Encode.Value )
listField name items encoder =
    if List.isEmpty items then
        []

    else
        [ ( name, Encode.list encoder items ) ]


{-| Produce a doc field entry only when the string is non-empty. -}
nonEmptyDocField : String -> List ( String, Encode.Value )
nonEmptyDocField doc =
    if String.isEmpty doc then
        []

    else
        [ ( "doc", Encode.string doc ) ]


encodeLiteral : Literal -> Encode.Value
encodeLiteral lit =
    case lit of
        BoolLiteral v ->
            encodeTaggedValue "Bool" (Encode.bool v)

        CharLiteral v ->
            encodeTaggedValue "Char" (Encode.string (String.fromChar v))

        StringLiteral v ->
            encodeTaggedValue "String" (Encode.string v)

        WholeNumberLiteral v ->
            encodeTaggedValue "Int" (Encode.int v)

        FloatLiteral v ->
            encodeTaggedValue "Float" (Encode.float v)

        DecimalLiteral v ->
            encodeTaggedValue "Decimal" (Encode.string (Decimal.toString v))


encodeType : Type () -> Encode.Value
encodeType tpe =
    case tpe of
        Type.Variable _ name ->
            encodeTaggedValue "Variable" (encodeName name)

        Type.Reference _ typeName typeParameters ->
            case listField "params" typeParameters encodeType of
                [] ->
                    encodeTaggedValue "Reference" (encodeFQName typeName)

                typeParamFields ->
                    encodeTaggedUnion "Reference"
                        (( "name", encodeFQName typeName ) :: typeParamFields)

        Type.Tuple _ elementTypes ->
            encodeTaggedValue "Tuple" (Encode.list encodeType elementTypes)

        Type.Record _ fieldTypes ->
            encodeTaggedValue "Record"
                (fieldTypes
                    |> List.map (\f -> ( Name.toTitleCase f.name, encodeType f.tpe ))
                    |> Encode.object
                )

        Type.ExtensibleRecord _ variableName fieldTypes ->
            encodeTaggedUnion "ExtensibleRecord"
                (( "variable", encodeName variableName )
                    :: List.map (\f -> ( Name.toTitleCase f.name, encodeType f.tpe )) fieldTypes
                )

        Type.Function _ argumentType returnType ->
            encodeTaggedUnion "Function"
                [ ( "from", encodeType argumentType )
                , ( "to", encodeType returnType )
                ]

        Type.Unit _ ->
            encodeTaggedUnion "Unit" []


encodeTypeDefinition : String -> Type.Definition () -> Encode.Value
encodeTypeDefinition doc def =
    case def of
        Type.TypeAliasDefinition params exp ->
            let
                extras =
                    nonEmptyDocField doc ++ listField "params" params encodeName
            in
            if List.isEmpty extras then
                encodeTaggedValue "Alias" (encodeType exp)

            else
                encodeTaggedUnion "Alias"
                    (extras ++ [ ( "type", encodeType exp ) ])

        Type.CustomTypeDefinition params ctors ->
            let
                extras =
                    nonEmptyDocField doc ++ listField "params" params encodeName
            in
            if List.isEmpty extras then
                encodeTaggedValue "Union" (encodeConstructors ctors.value)

            else
                encodeTaggedUnion "Union"
                    (extras ++ [ ( "constructors", encodeConstructors ctors.value ) ])


encodeConstructors : Constructors () -> Encode.Value
encodeConstructors ctors =
    ctors
        |> Dict.toList
        |> List.map
            (\( ctorName, ctorArgs ) ->
                ( Name.toTitleCase ctorName
                , ctorArgs
                    |> List.map (\( argName, argType ) -> ( Name.toTitleCase argName, encodeType argType ))
                    |> Encode.object
                )
            )
        |> Encode.object


{-| Walk left through a chain of curried Apply nodes, accumulating arguments.
Returns the innermost non-Apply function and the full argument list in call order.
-}
unwindApply : Value () (Type ()) -> List (Value () (Type ())) -> ( Value () (Type ()), List (Value () (Type ())) )
unwindApply v accArgs =
    case v of
        Value.Apply _ function argument ->
            unwindApply function (argument :: accArgs)

        _ ->
            ( v, accArgs )


encodeValue : Value () (Type ()) -> Encode.Value
encodeValue v =
    case v of
        Value.Literal _ lit ->
            encodeLiteral lit

        Value.Constructor _ fullyQualifiedName ->
            encodeTaggedValue "Constructor" (encodeFQName fullyQualifiedName)

        Value.Tuple _ elements ->
            encodeTaggedValue "Tuple" (Encode.list encodeValue elements)

        Value.List _ items ->
            encodeTaggedValue "List" (Encode.list encodeValue items)

        Value.Record _ fields ->
            encodeTaggedValue "Record"
                (fields
                    |> Dict.toList
                    |> List.map (\( fieldName, fieldValue ) -> ( Name.toTitleCase fieldName, encodeValue fieldValue ))
                    |> Encode.object
                )

        Value.Variable _ name ->
            encodeTaggedValue "Variable" (encodeName name)

        Value.Reference _ fullyQualifiedName ->
            encodeTaggedValue "Reference" (encodeFQName fullyQualifiedName)

        Value.Field _ subjectValue fieldName ->
            encodeTaggedUnion "Field"
                [ ( "on", encodeValue subjectValue )
                , ( "field", encodeName fieldName )
                ]

        Value.FieldFunction _ fieldName ->
            encodeTaggedValue "FieldFunction" (encodeName fieldName)

        Value.Apply _ function argument ->
            let
                ( fun, args ) =
                    unwindApply function [ argument ]
            in
            encodeTaggedValue "Apply"
                (Encode.list identity (encodeValue fun :: List.map encodeValue args))

        Value.Lambda _ argumentPattern body ->
            encodeTaggedUnion "Lambda"
                [ ( "arg", encodePattern argumentPattern )
                , ( "body", encodeValue body )
                ]

        Value.LetDefinition _ valueName valueDefinition inValue ->
            encodeTaggedUnion "Let"
                [ ( "name", encodeName valueName )
                , ( "def", encodeValueDefinition Nothing valueDefinition )
                , ( "in", encodeValue inValue )
                ]

        Value.LetRecursion _ valueDefinitions inValue ->
            encodeTaggedUnion "LetRec"
                [ ( "defs"
                  , valueDefinitions
                        |> Dict.toList
                        |> List.map (\( name, def ) -> ( Name.toTitleCase name, encodeValueDefinition Nothing def ))
                        |> Encode.object
                  )
                , ( "in", encodeValue inValue )
                ]

        Value.Destructure _ pattern valueToDestruct inValue ->
            encodeTaggedUnion "Destructure"
                [ ( "pattern", encodePattern pattern )
                , ( "value", encodeValue valueToDestruct )
                , ( "in", encodeValue inValue )
                ]

        Value.IfThenElse _ condition thenBranch elseBranch ->
            encodeTaggedUnion "If"
                [ ( "cond", encodeValue condition )
                , ( "then", encodeValue thenBranch )
                , ( "else", encodeValue elseBranch )
                ]

        Value.PatternMatch _ branchOutOn cases ->
            encodeTaggedUnion "Match"
                ([ ( "on", encodeValue branchOutOn ) ]
                    ++ listField "cases"
                        cases
                        (\( pattern, body ) ->
                            Encode.list identity [ encodePattern pattern, encodeValue body ]
                        )
                )

        Value.UpdateRecord _ valueToUpdate fieldsToUpdate ->
            encodeTaggedUnion "Update"
                (( "subject", encodeValue valueToUpdate )
                    :: (fieldsToUpdate
                            |> Dict.toList
                            |> List.map (\( fieldName, fieldValue ) -> ( Name.toTitleCase fieldName, encodeValue fieldValue ))
                       )
                )

        Value.Unit _ ->
            encodeTaggedUnion "Unit" []


encodePattern : Pattern (Type ()) -> Encode.Value
encodePattern pattern =
    case pattern of
        Value.WildcardPattern _ ->
            Encode.string "_"

        Value.AsPattern _ p name ->
            case p of
                Value.WildcardPattern _ ->
                    encodeTaggedValue "As" (encodeName name)

                _ ->
                    encodeTaggedUnion "As"
                        [ ( "pattern", encodePattern p )
                        , ( "name", encodeName name )
                        ]

        Value.TuplePattern _ elementPatterns ->
            encodeTaggedValue "Tuple" (Encode.list encodePattern elementPatterns)

        Value.ConstructorPattern _ constructorName argumentPatterns ->
            case argumentPatterns of
                [] ->
                    encodeTaggedValue "Ctor" (encodeFQName constructorName)

                _ ->
                    encodeTaggedValue "Ctor"
                        (Encode.object
                            [ ( FQName.toString constructorName
                              , Encode.list encodePattern argumentPatterns
                              )
                            ]
                        )

        Value.EmptyListPattern _ ->
            Encode.string "[]"

        Value.HeadTailPattern _ headPattern tailPattern ->
            encodeTaggedUnion "HeadTail"
                [ ( "head", encodePattern headPattern )
                , ( "tail", encodePattern tailPattern )
                ]

        Value.LiteralPattern _ lit ->
            encodeTaggedValue "Literal" (encodeLiteral lit)

        Value.UnitPattern _ ->
            Encode.string "()"


encodeValueDefinition : Maybe String -> Value.Definition () (Type ()) -> Encode.Value
encodeValueDefinition maybeDoc def =
    Encode.object
        ((maybeDoc |> Maybe.map nonEmptyDocField |> Maybe.withDefault [])
            ++ listField "inputs"
                def.inputTypes
                (\( argName, _, argType ) ->
                    Encode.object [ ( Name.toTitleCase argName, encodeType argType ) ]
                )
            ++ [ ( "returns", encodeType def.outputType )
               , ( "body", encodeValue def.body )
               ]
        )


encodeModuleDefinition : Module.Definition () (Type ()) -> Encode.Value
encodeModuleDefinition def =
    let
        typeEntries =
            def.types
                |> Dict.toList
                |> List.map
                    (\( name, typeDef ) ->
                        ( Name.toTitleCase name
                        , encodeTypeDefinition typeDef.value.doc typeDef.value.value
                        )
                    )

        valueEntries =
            def.values
                |> Dict.toList
                |> List.map
                    (\( name, valueDef ) ->
                        ( Name.toTitleCase name
                        , encodeValueDefinition (Just valueDef.value.doc) valueDef.value.value
                        )
                    )
    in
    Encode.object
        ((if List.isEmpty typeEntries then [] else [ ( "types", Encode.object typeEntries ) ])
            ++ (if List.isEmpty valueEntries then [] else [ ( "values", Encode.object valueEntries ) ])
            ++ (def.doc |> Maybe.map nonEmptyDocField |> Maybe.withDefault [])
        )
