module Morphir.Type.ConstraintSet exposing (..)

import Dict exposing (Dict)
import Morphir.Type.Constraint as Constraint exposing (Constraint(..))
import Morphir.Type.MetaType exposing (MetaType(..), Variable)
import Morphir.Type.Solve exposing (SolutionMap(..))


type ConstraintSet
    = ConstraintSet (List Constraint) (Dict Variable Variable)


empty : ConstraintSet
empty =
    ConstraintSet [] Dict.empty


singleton : Constraint -> ConstraintSet
singleton constraint =
    insert constraint empty


isEmpty : ConstraintSet -> Bool
isEmpty (ConstraintSet constraints aliases) =
    List.isEmpty constraints


member : Constraint -> ConstraintSet -> Bool
member constraint (ConstraintSet constraints aliases) =
    let
        dealiasedConstraint =
            constraint
                |> Constraint.substituteVariables
                    (aliases
                        |> Dict.map (\_ var2 -> MetaVar var2)
                    )
    in
    List.any (Constraint.equivalent dealiasedConstraint) constraints


insert : Constraint -> ConstraintSet -> ConstraintSet
insert constraint ((ConstraintSet constraints aliases) as constraintSet) =
    case constraint of
        Equality (MetaVar var1) (MetaVar var2) ->
            if var1 == var2 then
                constraintSet

            else
                ConstraintSet
                    (constraints
                        |> substituteVariableHelp var1 (MetaVar var2)
                    )
                    (case aliases |> Dict.get var2 of
                        Just var2alias ->
                            if var2alias == var1 then
                                aliases

                            else
                                aliases
                                    |> Dict.insert var1 var2

                        Nothing ->
                            aliases
                                |> Dict.insert var1 var2
                    )

        _ ->
            let
                dealiasedConstraint =
                    constraint
                        |> Constraint.substituteVariables
                            (aliases
                                |> Dict.map (\_ var2 -> MetaVar var2)
                            )
            in
            if Constraint.isTrivial dealiasedConstraint || member dealiasedConstraint constraintSet then
                constraintSet

            else
                ConstraintSet (dealiasedConstraint :: constraints) aliases


fromList : List Constraint -> ConstraintSet
fromList list =
    List.foldl insert empty list


toList : ConstraintSet -> List Constraint
toList (ConstraintSet constraints aliases) =
    List.concat
        [ constraints
        , aliases
            |> Dict.toList
            |> List.map (\( var1, var2 ) -> Equality (MetaVar var1) (MetaVar var2))
        ]


union : ConstraintSet -> ConstraintSet -> ConstraintSet
union constraintSet1 constraintSet2 =
    List.foldl insert constraintSet1 (toList constraintSet2)


concat : List ConstraintSet -> ConstraintSet
concat constraintSets =
    List.foldl union empty constraintSets


substituteVariable : Variable -> MetaType -> ConstraintSet -> ConstraintSet
substituteVariable var replacement (ConstraintSet constraints aliases) =
    ConstraintSet
        (substituteVariableHelp var replacement constraints)
        aliases


substituteVariableHelp : Variable -> MetaType -> List Constraint -> List Constraint
substituteVariableHelp var replacement constraints =
    constraints
        |> List.filterMap
            (\constraint ->
                let
                    newConstraint =
                        Constraint.substituteVariable var replacement constraint
                in
                if Constraint.isTrivial newConstraint then
                    Nothing

                else
                    Just newConstraint
            )


applySubstitutions : SolutionMap -> ConstraintSet -> ConstraintSet
applySubstitutions (SolutionMap substitutions) constraintSet =
    substitutions
        |> Dict.toList
        |> List.foldl
            (\( var, replacement ) soFar ->
                soFar |> substituteVariable var replacement
            )
            constraintSet
