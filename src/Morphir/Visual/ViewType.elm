module Morphir.Visual.ViewType exposing (..)

import Dict exposing (Dict)
import Element exposing (Element, el, none, padding, paddingEach, row, shrink, spacing, table, text)
import Morphir.IR.Attribute exposing (NodePath(..))
import Morphir.IR.FQName exposing (FQName)
import Morphir.IR.Name exposing (Name)
import Morphir.IR.Type as Type exposing (Type)
import Morphir.Visual.Common exposing (nameToText)


type alias Config ta =
    { lookupTypeDefinition : FQName -> Maybe (Type.Definition ta)
    , expandedNodes : ExpandedNodes
    }


type ExpandedNodes
    = ExpandedChildrenByName (Dict Name ExpandedNodes)
    | ExpandedChildrenByIndex (Dict Int ExpandedNodes)


type alias TreeTableRow msg =
    { hierarchyControls : Element msg
    , occurs : Element msg
    , typeName : Element msg
    }


view : Config ta -> Type.Definition ta -> Element msg
view config typeDef =
    table []
        { data =
            viewTypeDefinition config
                RootNode
                typeDef
        , columns =
            [ { header = none
              , width = shrink
              , view =
                    \treeTableRow ->
                        el [ padding 5 ]
                            treeTableRow.hierarchyControls
              }
            , { header = none
              , width = shrink
              , view =
                    \treeTableRow ->
                        el [ padding 5 ]
                            treeTableRow.occurs
              }
            , { header = none
              , width = shrink
              , view =
                    \treeTableRow ->
                        el [ padding 5 ]
                            treeTableRow.typeName
              }
            ]
        }


viewTypeDefinition : Config ta -> NodePath -> Type.Definition ta -> List (TreeTableRow msg)
viewTypeDefinition config nodePath typeDef =
    case typeDef of
        Type.TypeAliasDefinition typeArgs typeExp ->
            let
                children =
                    viewTypeChildren config nodePath typeExp
            in
            if List.isEmpty children then
                viewNodePathAndType config nodePath typeExp

            else
                children

        Type.CustomTypeDefinition typeArgs accessControlledConstructors ->
            accessControlledConstructors.value
                |> Dict.toList
                |> List.concatMap
                    (\( ctorName, ctorArgs ) ->
                        viewConstructor config (nodePath |> ChildNodeByName ctorName) ctorArgs
                    )


viewTypeOccurs : Config ta -> NodePath -> Type ta -> Element msg
viewTypeOccurs config nodePath tpe =
    case tpe of
        Type.Reference a ( [ [ "morphir" ], [ "s", "d", "k" ] ], [ [ "maybe" ] ], [ "maybe" ] ) [ elemType ] ->
            text "?"

        Type.Reference a ( [ [ "morphir" ], [ "s", "d", "k" ] ], [ [ "list" ] ], [ "list" ] ) [ elemType ] ->
            text "*"

        _ ->
            text "!"


viewTypeHeader : Config ta -> NodePath -> Type ta -> Element msg
viewTypeHeader config nodePath tpe =
    case tpe of
        Type.Variable a name ->
            text (nameToText name)

        Type.Reference a ( [ [ "morphir" ], [ "s", "d", "k" ] ], [ [ "maybe" ] ], [ "maybe" ] ) [ elemType ] ->
            viewTypeHeader config nodePath elemType

        Type.Reference a ( [ [ "morphir" ], [ "s", "d", "k" ] ], [ [ "list" ] ], [ "list" ] ) [ elemType ] ->
            viewTypeHeader config nodePath elemType

        Type.Reference a ( packageName, moduleName, localName ) types ->
            text (nameToText localName)

        Type.Tuple a elemTypes ->
            text "( ... )"

        Type.Record a fields ->
            text "{ ... }"

        Type.ExtensibleRecord a name fields ->
            text "{ e | ... }"

        Type.Function a argType returnType ->
            text "->"

        Type.Unit a ->
            text "()"


viewTypeChildren : Config ta -> NodePath -> Type ta -> List (TreeTableRow msg)
viewTypeChildren config nodePath tpe =
    case tpe of
        Type.Variable a name ->
            []

        Type.Reference a fQName types ->
            []

        Type.Tuple a elemTypes ->
            elemTypes
                |> List.indexedMap
                    (\elemIndex elemType ->
                        viewNodePathAndType config (nodePath |> ChildNodeByIndex elemIndex) elemType
                    )
                |> List.concat

        Type.Record a fields ->
            fields
                |> List.concatMap
                    (\field ->
                        viewNodePathAndType config (nodePath |> ChildNodeByName field.name) field.tpe
                    )

        Type.ExtensibleRecord a name fields ->
            fields
                |> List.concatMap
                    (\field ->
                        viewNodePathAndType config (nodePath |> ChildNodeByName field.name) field.tpe
                    )

        Type.Function a argType returnType ->
            List.concat
                [ viewNodePathAndType config (nodePath |> ChildNodeByName [ "input" ]) argType
                , viewNodePathAndType config (nodePath |> ChildNodeByName [ "output" ]) returnType
                ]

        Type.Unit a ->
            []


viewConstructor : Config ta -> NodePath -> List ( Name, Type ta ) -> List (TreeTableRow msg)
viewConstructor config nodePath args =
    let
        constructorRow : TreeTableRow msg
        constructorRow =
            { hierarchyControls = nodePathToEdgeLabel nodePath
            , occurs = none
            , typeName = none
            }

        argumentRows : List (TreeTableRow msg)
        argumentRows =
            args
                |> List.map
                    (\( argName, argType ) ->
                        { hierarchyControls = nodePathToEdgeLabel (nodePath |> ChildNodeByName argName)
                        , occurs = viewTypeOccurs config nodePath argType
                        , typeName = viewTypeHeader config nodePath argType
                        }
                    )
    in
    constructorRow :: argumentRows


viewNodePathAndType : Config ta -> NodePath -> Type ta -> List (TreeTableRow msg)
viewNodePathAndType config nodePath tpe =
    let
        headerRow =
            { hierarchyControls = nodePathToEdgeLabel nodePath
            , occurs = viewTypeOccurs config nodePath tpe
            , typeName = viewTypeHeader config nodePath tpe
            }

        childRows =
            viewTypeChildren config nodePath tpe
    in
    headerRow :: childRows


nodePathToEdgeLabel : NodePath -> Element msg
nodePathToEdgeLabel nodePath =
    let
        nodePathLength : Int -> NodePath -> Int
        nodePathLength lengthSoFar path =
            case path of
                RootNode ->
                    lengthSoFar

                ChildNodeByName _ restOfPath ->
                    nodePathLength (lengthSoFar + 1) restOfPath

                ChildNodeByIndex _ restOfPath ->
                    nodePathLength (lengthSoFar + 1) restOfPath

        indentRight : Int -> Element.Attribute msg
        indentRight by =
            paddingEach
                { left = by * 20
                , right = 0
                , top = 0
                , bottom = 0
                }

        viewLabel labelText restOfPath =
            row
                [ indentRight (nodePathLength 0 restOfPath)
                , spacing 5
                ]
                [ text "⮞"
                , text labelText
                ]
    in
    case nodePath of
        RootNode ->
            none

        ChildNodeByName name restOfPath ->
            viewLabel (nameToText name) restOfPath

        ChildNodeByIndex index restOfPath ->
            viewLabel (String.fromInt index) restOfPath
