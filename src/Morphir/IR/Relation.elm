module Morphir.IR.Relation exposing (..)

import Dict exposing (Dict)
import Morphir.IR.Name exposing (Name)
import Morphir.IR.Value exposing (Value)


type alias ObjectExp a =
    Value () a


type alias ColumnExp a =
    Value () a


type Relation a
    = From (ObjectExp a) Name
    | Where (ColumnExp a) (Relation a)
    | Select (Dict Name (ColumnExp a)) (Relation a)
    | Join JoinType (ColumnExp a) (Relation a) (Relation a)
    | GroupBy (List (ColumnExp a)) (Relation a)


type JoinType
    = Inner
    | Outer JoinDirection


type JoinDirection
    = Left
    | Right
    | Full


from : ObjectExp a -> Name -> Relation a
from source alias =
    From source alias


where_ : ColumnExp a -> Relation a -> Relation a
where_ predicate source =
    Where predicate source


select : Dict Name (Value () a) -> Relation a -> Relation a
select fields source =
    Select fields source


join : JoinType -> ColumnExp a -> Relation a -> Relation a -> Relation a
join joinType on left right =
    Join joinType on left right


groupBy : List (ColumnExp a) -> Relation a -> Relation a
groupBy keys source =
    GroupBy keys source
