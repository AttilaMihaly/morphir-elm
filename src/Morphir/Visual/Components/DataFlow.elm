module Morphir.Visual.Components.DataFlow exposing (..)

import Element exposing (Element, el, height, html, px, width)
import Html
import Html.Attributes
import Json.Encode as Encode
import Morphir.IR.Value as Value
import Morphir.Visual.Common exposing (VisualTypedValue, nameToText)


viewDataFlowChart : VisualTypedValue -> List VisualTypedValue -> Element msg
viewDataFlowChart functionValue argValues =
    el [ width (px 600), height (px 400) ]
        (html
            (Html.node "data-flow-chart"
                [ Html.Attributes.property "data" (functionToChartData functionValue argValues) ]
                []
            )
        )


functionToChartData : VisualTypedValue -> List VisualTypedValue -> Encode.Value
functionToChartData functionValue argValues =
    Encode.object
        [ ( "name", Encode.string (valueToString functionValue) )
        , ( "children"
          , argValues
                |> List.filterMap valueToChartData
                |> Encode.list identity
          )
        ]


valueToChartData : VisualTypedValue -> Maybe Encode.Value
valueToChartData value =
    case value of
        Value.Variable _ name ->
            Just
                (Encode.object
                    [ ( "name", Encode.string (nameToText name) ) ]
                )

        Value.Apply _ fun arg ->
            let
                ( function, args ) =
                    Value.uncurryApply fun arg
            in
            Just (functionToChartData function args)

        other ->
            Nothing


valueToString : VisualTypedValue -> String
valueToString value =
    case value of
        Value.Reference _ ( _, _, localName ) ->
            nameToText localName

        other ->
            Debug.toString other
