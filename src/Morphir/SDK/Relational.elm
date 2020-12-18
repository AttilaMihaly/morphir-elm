module Morphir.SDK.Relational exposing (..)


type Join a b
    = Inner (a -> Maybe b)
    | Outer (a -> Maybe b)


joinAndSelect : (a -> r1) -> (a -> r1 -> b) -> List a -> List b
joinAndSelect join1 select from =
    from
        |> List.map
            (\fromRow ->
                select fromRow (join1 fromRow)
            )


joinAndSelect2 : (a -> r1) -> (a -> r2) -> (a -> r1 -> r2 -> b) -> List a -> List b
joinAndSelect2 join1 join2 select from =
    from
        |> List.map
            (\fromRow ->
                select fromRow (join1 fromRow) (join2 fromRow)
            )


joinAndSelect3 : (a -> Maybe r1) -> (a -> Maybe r2) -> (a -> Maybe r3) -> (a -> r1 -> r2 -> r3 -> b) -> List a -> List b
joinAndSelect3 join1 join2 join3 select from =
    from
        |> List.filterMap
            (\fromRow ->
                Maybe.map3 (select fromRow)
                    (join1 fromRow)
                    (join2 fromRow)
                    (join3 fromRow)
            )


type alias Schema =
    { findGLAccount : String -> Maybe GLAccount
    }


type alias Position =
    { glAccountID : String
    }


type alias GLAccount =
    {}


usage schema =
    let
        withGlAccount : Position -> Maybe GLAccount
        withGlAccount position =
            schema.findGLAccount position.glAccountID

        withParty position =
            schema.party
                |> List.filter (\party -> party.partyID position.partyID)

        withCostCenter position =
            schema.party
                |> List.filter (\cc -> cc.ccID position.ccID)
    in
    schema.positions
        |> joinAndSelect3
            withGlAccount
            withParty
            withCostCenter
            (\position glAccount party costCenter ->
                { id = position.id
                , partyName = party.name
                }
            )
