module Morphir.TreePath exposing
    ( TreePath, descend
    , find
    , toString, toList
    )

{-| This is a data structure to capture a path in a tree as a series of child indices. It can be used as an identifier
for any node in the tree. To find the tree node denoted by the path we simply have to traverse the tree from the root
descending to the child with the next index from the path at each step until we use all the indices.

Here's a sample tree with the path at each node to demonstrate:

  - Root Node []
      - Node A - [0]
          - Node B - [0,0]
          - Node C - [0,1]
              - Node D - [0,1,0]
              - Node E - [0,1,1]
              - Node F - [0,1,2]
      - Node G - [1]
        -Node H - [1,0]


# Construction

@docs TreePath, descend


# Traversal

@docs find


# Conversions

@docs toString, toList

-}


{-| Type that represents a tree path.
-}
type alias TreePath =
    List Int


{-| Get a string representation of the path.

    toString [] == "/"

    toString [ 0 ] == "/0"

    toString [ 0, 1, 2 ] == "/0/1/2"

    toString [ 123, 500 ] == "/123/500"

-}
toString : TreePath -> String
toString treePath =
    treePath
        |> List.map String.fromInt
        |> String.join "/"
        |> (++) "/"


{-| Add a new step to an existing path.

    [] |> descend 1 == [ 1 ]

    [ 1 ] |> descend 2 == [ 1, 2 ]

-}
descend : Int -> TreePath -> TreePath
descend childIndex treePath =
    treePath ++ [ childIndex ]


{-| Convert a path into a list of indices.
-}
toList : TreePath -> List Int
toList treePath =
    treePath


{-| Find a node in a tree using its path. Any tree structure can be used as long as you can supply the root node and a
function to get the child at a specific index for each node.

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

    find sampleTree getChildAt [] == Just "Root"

    find sampleTree getChildAt [ 0, 1, 0 ] == Just "D"

    find sampleTree getChildAt [ 0, 1, 3 ] == Nothing

-}
find : treeNode -> (Int -> treeNode -> Maybe treeNode) -> TreePath -> Maybe treeNode
find rootNode getChildAt treePath =
    case treePath of
        [] ->
            Just rootNode

        nextIndex :: restOfPath ->
            getChildAt nextIndex rootNode
                |> Maybe.andThen (\child -> find child getChildAt restOfPath)
