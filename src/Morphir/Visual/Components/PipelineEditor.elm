module Morphir.Visual.Components.PipelineEditor exposing (..)

import Element exposing (Element, alignRight, column, el, fill, none, padding, rgb, row, text, width)
import Element.Border as Border


type Pipeline a
    = ProcessingNode a (List (Pipeline a))
    | SourceNode a


viewPipeline : (a -> Element msg) -> Pipeline a -> Element msg
viewPipeline viewA pipeline =
    case pipeline of
        ProcessingNode content sources ->
            row []
                [ sources
                    |> List.indexedMap
                        (\index source ->
                            row [ alignRight ]
                                [ viewPipeline viewA source
                                , viewArrow (List.length sources) index
                                ]
                        )
                    |> column []
                , viewNode (viewA content)
                ]

        SourceNode content ->
            viewNode (viewA content)


viewNode : Element msg -> Element msg
viewNode content =
    el
        [ padding 5
        , Border.color (rgb 0.5 0.5 0.5)
        , Border.width 2
        , Border.rounded 5
        ]
        content


viewArrow : Int -> Int -> Element msg
viewArrow length index =
    if index == 0 && length > 1 then
        -- top branch
        text "--+-\\"

    else if index < (length - 1) || length == 1 then
        -- middle branch
        text "--+->"

    else
        -- bottom branch
        text "--+-/"
