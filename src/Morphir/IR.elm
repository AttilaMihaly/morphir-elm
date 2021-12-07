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


module Morphir.IR exposing
    ( IR
    , insertValueDefinition, insertTypeDefinition, insertModuleDefinition
    , insertValueSpecification, insertTypeSpecification, insertModuleSpecification
    , fromPackageSpecifications, fromDistribution, toDistribution
    , lookupTypeSpecification, lookupTypeConstructor, lookupValueSpecification, lookupValueDefinition
    , empty, union, resolveAliases, resolveType, resolveRecordConstructors, moduleSpecifications
    )

{-| This module contains data structures and functions to make working with the IR easier and more efficient.

@docs IR


# Write Operations

@docs insertValueDefinition, insertTypeDefinition, insertModuleDefinition
@docs insertValueSpecification, insertTypeSpecification, insertModuleSpecification


# Conversions

@docs fromPackageSpecifications, fromDistribution, toDistribution


# Lookups

@docs lookupTypeSpecification, lookupTypeConstructor, lookupValueSpecification, lookupValueDefinition


# Utilities

@docs empty, union, resolveAliases, resolveType, resolveRecordConstructors, moduleSpecifications

-}

import Dict exposing (Dict)
import Morphir.IR.AccessControlled exposing (AccessControlled, private, public)
import Morphir.IR.Distribution as Distribution exposing (Distribution(..))
import Morphir.IR.Documented exposing (Documented)
import Morphir.IR.FQName exposing (FQName)
import Morphir.IR.Module as Module exposing (ModuleName)
import Morphir.IR.Name exposing (Name)
import Morphir.IR.Package as Package exposing (PackageName)
import Morphir.IR.Type as Type exposing (Type)
import Morphir.IR.Value as Value exposing (Value)
import Set exposing (Set)


{-| Data structure to store types and values efficiently.
-}
type alias IR =
    { valueSpecifications : Dict FQName (Value.Specification ())
    , valueDefinitions : Dict FQName (Value.Definition () (Type ()))
    , typeSpecifications : Dict FQName (Type.Specification ())
    , typeConstructors : Dict FQName ( FQName, List Name, List ( Name, Type () ) )
    }


{-| Creates and empty IR with no types or values.
-}
empty : IR
empty =
    { valueSpecifications = Dict.empty
    , valueDefinitions = Dict.empty
    , typeSpecifications = Dict.empty
    , typeConstructors = Dict.empty
    }


{-| Combine two IRs to include all the definitions from both. In case of clashes the definition in the first IR will be
retained.
-}
union : IR -> IR -> IR
union ir1 ir2 =
    { valueSpecifications =
        Dict.union
            ir1.valueSpecifications
            ir2.valueSpecifications
    , valueDefinitions =
        Dict.union
            ir1.valueDefinitions
            ir2.valueDefinitions
    , typeSpecifications =
        Dict.union
            ir1.typeSpecifications
            ir2.typeSpecifications
    , typeConstructors =
        Dict.union
            ir1.typeConstructors
            ir2.typeConstructors
    }


{-| Insert a type definition into the IR.
-}
insertTypeSpecification : FQName -> Type.Specification () -> IR -> IR
insertTypeSpecification (( packageName, moduleName, typeName ) as fQName) typeSpec ir =
    { ir
        | typeSpecifications =
            ir.typeSpecifications
                |> Dict.insert (fQName |> Debug.log "insert type") typeSpec
    }


{-| Insert a type definition into the IR.
-}
insertTypeDefinition : FQName -> Type.Definition () -> IR -> IR
insertTypeDefinition (( packageName, moduleName, typeName ) as fQName) typeDef ir =
    { ir
        | typeSpecifications =
            ir.typeSpecifications
                |> Dict.insert (fQName |> Debug.log "insert type") (Type.definitionToSpecification typeDef)
        , typeConstructors =
            case typeDef of
                Type.CustomTypeDefinition params constructors ->
                    constructors.value
                        |> Dict.toList
                        |> List.foldl
                            (\( ctorName, ctorArgs ) typeConstructorsSoFar ->
                                typeConstructorsSoFar
                                    |> Dict.insert ( packageName, moduleName, ctorName )
                                        ( fQName, params, ctorArgs )
                            )
                            ir.typeConstructors

                _ ->
                    ir.typeConstructors
    }


{-| Insert a value specification into the IR.
-}
insertValueSpecification : FQName -> Value.Specification () -> IR -> IR
insertValueSpecification fQName valueSpec ir =
    { ir
        | valueSpecifications =
            ir.valueSpecifications
                |> Dict.insert (fQName |> Debug.log "insert value") valueSpec
    }


{-| Insert a value definition into the IR.
-}
insertValueDefinition : FQName -> Value.Definition () (Type ()) -> IR -> IR
insertValueDefinition fQName valueDef ir =
    { ir
        | valueDefinitions =
            ir.valueDefinitions
                |> Dict.insert (fQName |> Debug.log "insert value") valueDef
    }


{-| Insert a module specification into the IR.
-}
insertModuleSpecification : PackageName -> ModuleName -> Module.Specification () -> IR -> IR
insertModuleSpecification packageName moduleName moduleSpec =
    let
        insertTypes : IR -> IR
        insertTypes ir =
            moduleSpec.types
                |> Dict.toList
                |> List.foldl
                    (\( typeName, typeSpec ) irSoFar ->
                        irSoFar
                            |> insertTypeSpecification ( packageName, moduleName, typeName ) typeSpec.value
                    )
                    ir

        insertValues : IR -> IR
        insertValues ir =
            moduleSpec.values
                |> Dict.toList
                |> List.foldl
                    (\( valueName, valueSpec ) irSoFar ->
                        irSoFar
                            |> insertValueSpecification ( packageName, moduleName, valueName ) valueSpec
                    )
                    ir
    in
    insertTypes >> insertValues


{-| Insert a module definition into the IR.
-}
insertModuleDefinition : PackageName -> ModuleName -> Module.Definition () (Type ()) -> IR -> IR
insertModuleDefinition packageName moduleName moduleDef =
    let
        insertTypes : IR -> IR
        insertTypes ir =
            moduleDef.types
                |> Dict.toList
                |> List.foldl
                    (\( typeName, typeDef ) irSoFar ->
                        irSoFar
                            |> insertTypeDefinition ( packageName, moduleName, typeName ) typeDef.value.value
                    )
                    ir

        insertValues : IR -> IR
        insertValues ir =
            moduleDef.values
                |> Dict.toList
                |> List.foldl
                    (\( valueName, valueDef ) irSoFar ->
                        irSoFar
                            |> insertValueDefinition ( packageName, moduleName, valueName ) valueDef.value
                    )
                    ir
    in
    insertTypes >> insertValues


{-| Turn a `Distribution` into an `IR`. The `Distribution` data type is optimized for transfer while the `IR` data type
is optimized for efficient in-memory processing.
-}
fromDistribution : Distribution -> IR
fromDistribution (Distribution.Library libraryName dependencies packageDef) =
    let
        packageSpecs : Dict PackageName (Package.Specification ())
        packageSpecs =
            dependencies
                |> Dict.insert libraryName (packageDef |> Package.definitionToSpecificationWithPrivate)

        specificationsOnly : IR
        specificationsOnly =
            fromPackageSpecifications packageSpecs

        packageValueDefinitions : Dict FQName (Value.Definition () (Type ()))
        packageValueDefinitions =
            packageDef.modules
                |> Dict.toList
                |> List.concatMap
                    (\( moduleName, moduleDef ) ->
                        moduleDef.value.values
                            |> Dict.toList
                            |> List.map
                                (\( valueName, valueDef ) ->
                                    ( ( libraryName, moduleName, valueName ), valueDef.value )
                                )
                    )
                |> Dict.fromList
    in
    { specificationsOnly
        | valueDefinitions = packageValueDefinitions
    }


{-| Extracts a distribution from th IR. Since the IR contains multiple packages, the package name to be extracted needs
to be specified.
-}
toDistribution : PackageName -> IR -> Distribution
toDistribution packageName ir =
    let
        moduleDefs : Dict ModuleName (AccessControlled (Module.Definition () (Type ())))
        moduleDefs =
            moduleNames packageName ir
                |> List.map
                    (\moduleName ->
                        let
                            types : Dict Name (AccessControlled (Documented (Type.Definition ())))
                            types =
                                ir.typeSpecifications
                                    |> Dict.toList
                                    |> List.filterMap
                                        (\( ( p, m, typeName ), typeSpec ) ->
                                            let
                                                typeDef : Type.Definition ()
                                                typeDef =
                                                    case typeSpec of
                                                        Type.TypeAliasSpecification params typeExp ->
                                                            Type.TypeAliasDefinition params typeExp

                                                        Type.OpaqueTypeSpecification params ->
                                                            let
                                                                constructors : Dict Name (List ( Name, Type () ))
                                                                constructors =
                                                                    ir.typeConstructors
                                                                        |> Dict.toList
                                                                        |> List.filterMap
                                                                            (\( ( cp, cm, cn ), ( ( _, _, tn ), _, args ) ) ->
                                                                                if cp == packageName && cm == moduleName && tn == typeName then
                                                                                    Just ( cn, args )

                                                                                else
                                                                                    Nothing
                                                                            )
                                                                        |> Dict.fromList
                                                            in
                                                            Type.CustomTypeDefinition params (private constructors)

                                                        Type.CustomTypeSpecification params constructors ->
                                                            Type.CustomTypeDefinition params (public constructors)
                                            in
                                            if p == packageName && m == moduleName then
                                                Just ( typeName, public { doc = "", value = typeDef } )

                                            else
                                                Nothing
                                        )
                                    |> Dict.fromList

                            values : Dict Name (AccessControlled (Value.Definition () (Type ())))
                            values =
                                ir.valueDefinitions
                                    |> Dict.toList
                                    |> List.filterMap
                                        (\( ( p, m, valueName ), valueDef ) ->
                                            if p == packageName && m == moduleName then
                                                Just ( valueName, public valueDef )

                                            else
                                                Nothing
                                        )
                                    |> Dict.fromList
                        in
                        ( moduleName
                        , public (Module.Definition types values)
                        )
                    )
                |> Dict.fromList

        packageDef : Package.Definition () (Type ())
        packageDef =
            Package.Definition moduleDefs
    in
    Library packageName Dict.empty packageDef


{-| Turn a dictionary of package specifications into an `IR`.
-}
fromPackageSpecifications : Dict PackageName (Package.Specification ()) -> IR
fromPackageSpecifications packageSpecs =
    let
        packageValueSpecifications : PackageName -> Package.Specification () -> List ( FQName, Value.Specification () )
        packageValueSpecifications packageName packageSpec =
            packageSpec.modules
                |> Dict.toList
                |> List.concatMap
                    (\( moduleName, moduleSpec ) ->
                        moduleSpec.values
                            |> Dict.toList
                            |> List.map
                                (\( valueName, valueSpec ) ->
                                    ( ( packageName, moduleName, valueName ), valueSpec )
                                )
                    )

        packageTypeSpecifications : PackageName -> Package.Specification () -> List ( FQName, Type.Specification () )
        packageTypeSpecifications packageName packageSpec =
            packageSpec.modules
                |> Dict.toList
                |> List.concatMap
                    (\( moduleName, moduleSpec ) ->
                        moduleSpec.types
                            |> Dict.toList
                            |> List.map
                                (\( typeName, typeSpec ) ->
                                    ( ( packageName, moduleName, typeName ), typeSpec.value )
                                )
                    )

        packageTypeConstructors : PackageName -> Package.Specification () -> List ( FQName, ( FQName, List Name, List ( Name, Type () ) ) )
        packageTypeConstructors packageName packageSpec =
            packageSpec.modules
                |> Dict.toList
                |> List.concatMap
                    (\( moduleName, moduleSpec ) ->
                        moduleSpec.types
                            |> Dict.toList
                            |> List.concatMap
                                (\( typeName, typeSpec ) ->
                                    case typeSpec.value of
                                        Type.CustomTypeSpecification params constructors ->
                                            constructors
                                                |> Dict.toList
                                                |> List.map
                                                    (\( ctorName, ctorArgs ) ->
                                                        ( ( packageName, moduleName, ctorName ), ( ( packageName, moduleName, typeName ), params, ctorArgs ) )
                                                    )

                                        _ ->
                                            []
                                )
                    )
    in
    { valueSpecifications = flattenPackages packageSpecs packageValueSpecifications
    , valueDefinitions = Dict.empty
    , typeSpecifications = flattenPackages packageSpecs packageTypeSpecifications
    , typeConstructors = flattenPackages packageSpecs packageTypeConstructors
    }


flattenPackages : Dict PackageName p -> (PackageName -> p -> List ( FQName, r )) -> Dict FQName r
flattenPackages packages f =
    packages
        |> Dict.toList
        |> List.concatMap
            (\( packageName, package ) ->
                f packageName package
            )
        |> Dict.fromList


{-| Look up a value specification by fully-qualified name. Dependencies will be included in the search.
-}
lookupValueSpecification : FQName -> IR -> Maybe (Value.Specification ())
lookupValueSpecification fqn ir =
    ir.valueSpecifications
        |> Dict.get fqn


{-| Look up a value definition by fully-qualified name. Dependencies will not be included in the search.
-}
lookupValueDefinition : FQName -> IR -> Maybe (Value.Definition () (Type ()))
lookupValueDefinition fqn ir =
    ir.valueDefinitions
        |> Dict.get fqn


{-| Look up a type specification by fully-qualified name. Dependencies will be included in the search.
-}
lookupTypeSpecification : FQName -> IR -> Maybe (Type.Specification ())
lookupTypeSpecification fqn ir =
    ir.typeSpecifications
        |> Dict.get fqn


{-| Look up a type constructor by fully-qualified name. Dependencies will be included in the search. The function
returns a tuple with the following elements:

  - The fully-qualified name of the type that this constructor belongs to.
  - The type arguments of the type.
  - The list of arguments (as name-type pairs) for this constructor.

-}
lookupTypeConstructor : FQName -> IR -> Maybe ( FQName, List Name, List ( Name, Type () ) )
lookupTypeConstructor fqn ir =
    ir.typeConstructors
        |> Dict.get fqn


{-| Follow direct aliases until the leaf type is found.
-}
resolveAliases : FQName -> IR -> FQName
resolveAliases fQName ir =
    ir
        |> lookupTypeSpecification fQName
        |> Maybe.map
            (\typeSpec ->
                case typeSpec of
                    Type.TypeAliasSpecification _ (Type.Reference _ aliasFQName _) ->
                        aliasFQName

                    _ ->
                        fQName
            )
        |> Maybe.withDefault fQName


{-| Fully resolve all type aliases in the type.
-}
resolveType : Type () -> IR -> Type ()
resolveType tpe ir =
    case tpe of
        Type.Variable a name ->
            Type.Variable a name

        Type.Reference _ fQName typeParams ->
            ir
                |> lookupTypeSpecification fQName
                |> Maybe.map
                    (\typeSpec ->
                        case typeSpec of
                            Type.TypeAliasSpecification typeParamNames targetType ->
                                Type.substituteTypeVariables
                                    (List.map2 Tuple.pair typeParamNames typeParams
                                        |> Dict.fromList
                                    )
                                    targetType

                            _ ->
                                tpe
                    )
                |> Maybe.withDefault tpe

        Type.Tuple a elemTypes ->
            Type.Tuple a (elemTypes |> List.map (\t -> resolveType t ir))

        Type.Record a fields ->
            Type.Record a (fields |> List.map (\f -> { f | tpe = resolveType f.tpe ir }))

        Type.ExtensibleRecord a varName fields ->
            Type.ExtensibleRecord a varName (fields |> List.map (\f -> { f | tpe = resolveType f.tpe ir }))

        Type.Function a argType returnType ->
            Type.Function a (resolveType argType ir) (resolveType returnType ir)

        Type.Unit a ->
            Type.Unit a


{-| Replace record constructors with the corresponding record value.
-}
resolveRecordConstructors : Value ta va -> IR -> Value ta va
resolveRecordConstructors value ir =
    value
        |> Value.rewriteValue
            (\v ->
                case v of
                    Value.Apply _ fun lastArg ->
                        let
                            ( bottomFun, args ) =
                                Value.uncurryApply fun lastArg
                        in
                        case bottomFun of
                            Value.Constructor va fqn ->
                                ir
                                    |> lookupTypeSpecification fqn
                                    |> Maybe.andThen
                                        (\typeSpec ->
                                            case typeSpec of
                                                Type.TypeAliasSpecification _ (Type.Record _ fields) ->
                                                    Just
                                                        (Value.Record va
                                                            (List.map2 Tuple.pair (fields |> List.map .name) args)
                                                        )

                                                _ ->
                                                    Nothing
                                        )

                            _ ->
                                Nothing

                    _ ->
                        Nothing
            )


{-| Get all the module specifications for a specific package.
-}
moduleSpecifications : PackageName -> IR -> Dict ModuleName (Module.Specification ())
moduleSpecifications packageName ir =
    moduleNames packageName ir
        |> List.map
            (\moduleName ->
                let
                    types : Dict Name (Documented (Type.Specification ()))
                    types =
                        ir.typeSpecifications
                            |> Dict.toList
                            |> List.filterMap
                                (\( ( p, m, typeName ), typeSpec ) ->
                                    if p == packageName && m == moduleName then
                                        Just ( typeName, Documented "" typeSpec )

                                    else
                                        Nothing
                                )
                            |> Dict.fromList

                    values : Dict Name (Value.Specification ())
                    values =
                        ir.valueSpecifications
                            |> Dict.toList
                            |> List.filterMap
                                (\( ( p, m, valueName ), valueSpec ) ->
                                    if p == packageName && m == moduleName then
                                        Just ( valueName, valueSpec )

                                    else
                                        Nothing
                                )
                            |> Dict.fromList
                in
                ( moduleName, Module.Specification types values )
            )
        |> Dict.fromList


moduleNames : PackageName -> IR -> List ModuleName
moduleNames packageName ir =
    List.concat
        [ ir.typeSpecifications
            |> Dict.keys
            |> List.filterMap
                (\( p, m, _ ) ->
                    if p == packageName then
                        Just m

                    else
                        Nothing
                )
        , ir.typeConstructors
            |> Dict.values
            |> List.filterMap
                (\( ( p, m, _ ), _, _ ) ->
                    if p == packageName then
                        Just m

                    else
                        Nothing
                )
        , ir.valueDefinitions
            |> Dict.keys
            |> List.filterMap
                (\( p, m, _ ) ->
                    if p == packageName then
                        Just m

                    else
                        Nothing
                )
        ]
        -- distinct
        |> Set.fromList
        |> Set.toList
