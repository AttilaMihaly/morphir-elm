module Morphir.TreePathTests exposing (..)

import Expect
import Morphir.TreePath as TreePath exposing (TreePath)
import Test exposing (Test, describe, test)


type TreeNode
    = TreeNode String (List TreeNode)


getChildAt : Int -> TreeNode -> Maybe TreeNode
getChildAt index (TreeNode _ children) =
    if index < 0 then
        Nothing

    else
        children
            |> List.drop index
            |> List.head


findTests : Test
findTests =
    let
        sampleTree : TreeNode
        sampleTree =
            TreeNode "Root"
                [ TreeNode "A"
                    [ TreeNode "B" []
                    , TreeNode "C"
                        [ TreeNode "D" []
                        , TreeNode "E" []
                        , TreeNode "F" []
                        ]
                    ]
                , TreeNode "G"
                    [ TreeNode "H" [] ]
                ]

        positive : String -> TreePath -> Test
        positive expectedLabel path =
            test ("label " ++ expectedLabel ++ " at path " ++ TreePath.toString path)
                (\_ ->
                    case TreePath.find sampleTree getChildAt path of
                        Just (TreeNode label _) ->
                            label |> Expect.equal expectedLabel

                        Nothing ->
                            Expect.fail ("No node found at " ++ TreePath.toString path)
                )

        negative : TreePath -> Test
        negative path =
            test ("path " ++ TreePath.toString path)
                (\_ ->
                    case TreePath.find sampleTree getChildAt path of
                        Just (TreeNode label _) ->
                            Expect.fail ("Expected no nodes here, but found one with label: " ++ label)

                        Nothing ->
                            Expect.pass
                )
    in
    describe "find"
        [ describe "do find"
            [ positive "Root" []
            , positive "A" [ 0 ]
            , positive "B" [ 0, 0 ]
            , positive "C" [ 0, 1 ]
            , positive "D" [ 0, 1, 0 ]
            , positive "E" [ 0, 1, 1 ]
            , positive "F" [ 0, 1, 2 ]
            , positive "G" [ 1 ]
            , positive "H" [ 1, 0 ]
            ]
        , describe "do not find"
            [ negative [ 2 ]
            , negative [ 0, 2 ]
            , negative [ -1 ]
            ]
        ]
