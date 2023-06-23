module Morphir.IR.Relation exposing (..)

import Dict exposing (Dict)
import Morphir.IR.Name exposing (Name)
import Morphir.IR.Value exposing (Value)


type alias ObjectExp a =
    Value () a


type alias ColumnExp a =
    Value () a


type Relation a
    = From a (ObjectExp a) Name
    | Where a (ColumnExp a) (Relation a)
    | Select a (Dict Name (ColumnExp a)) (Relation a)
    | Join a JoinType (ColumnExp a) (Relation a) (Relation a)
    | GroupBy a (List (ColumnExp a)) (Relation a)


type JoinType
    = Inner
    | Outer JoinDirection


type JoinDirection
    = Left
    | Right
    | Full


from : a -> ObjectExp a -> Name -> Relation a
from attribute source alias =
    From attribute source alias


where_ : a -> ColumnExp a -> Relation a -> Relation a
where_ attribute predicate source =
    Where attribute predicate source


select : a -> Dict Name (Value () a) -> Relation a -> Relation a
select attribute fields source =
    Select attribute fields source


join : a -> JoinType -> ColumnExp a -> Relation a -> Relation a -> Relation a
join attribute joinType on left right =
    Join attribute joinType on left right


groupBy : a -> List (ColumnExp a) -> Relation a -> Relation a
groupBy attribute keys source =
    GroupBy attribute keys source
