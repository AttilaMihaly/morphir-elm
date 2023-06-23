module Morphir.IR.Relation.Codec exposing (..)

import Dict
import Json.Encode as Encode
import Morphir.IR.Name.Codec exposing (encodeName)
import Morphir.IR.Relation exposing (JoinDirection(..), JoinType(..), Relation(..))
import Morphir.IR.Value exposing (Value)
import Morphir.IR.Value.Codec exposing (encodeValue)


encodeRelation : (a -> Encode.Value) -> Relation a -> Encode.Value
encodeRelation encodeA relation =
    let
        encodeValueExp : Value () a -> Encode.Value
        encodeValueExp value =
            encodeValue (always (Encode.object [])) encodeA value
    in
    case relation of
        From a subject alias ->
            Encode.list identity
                [ Encode.string "From"
                , encodeA a
                , encodeValueExp subject
                , encodeName alias
                ]

        Where a predicate source ->
            Encode.list identity
                [ Encode.string "Where"
                , encodeA a
                , encodeValueExp predicate
                , encodeRelation encodeA source
                ]

        Select a fields source ->
            Encode.list identity
                [ Encode.string "Select"
                , encodeA a
                , fields
                    |> Dict.toList
                    |> Encode.list
                        (\( fieldName, fieldValue ) ->
                            Encode.list identity
                                [ encodeName fieldName
                                , encodeValueExp fieldValue
                                ]
                        )
                , encodeRelation encodeA source
                ]

        Join a joinType on left right ->
            Encode.list identity
                [ Encode.string "Join"
                , encodeA a
                , encodeJoinType joinType
                , encodeValueExp on
                , encodeRelation encodeA left
                , encodeRelation encodeA right
                ]

        GroupBy a keys source ->
            Encode.list identity
                [ Encode.string "GroupBy"
                , encodeA a
                , keys
                    |> Encode.list encodeValueExp
                , encodeRelation encodeA source
                ]


encodeJoinType : JoinType -> Encode.Value
encodeJoinType joinType =
    case joinType of
        Inner ->
            Encode.string "Inner"

        Outer joinDirection ->
            Encode.list identity
                [ Encode.string "Outer"
                , encodeJoinDirection joinDirection
                ]


encodeJoinDirection : JoinDirection -> Encode.Value
encodeJoinDirection joinDirection =
    case joinDirection of
        Left ->
            Encode.string "Left"

        Right ->
            Encode.string "Right"

        Full ->
            Encode.string "Full"
