module Morphir.Visual.ViewValue exposing (viewDefinition)

import Dict exposing (Dict)
import Element exposing (Element, el, fill, htmlAttribute, padding, rgb, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (onClick, onMouseEnter, onMouseLeave)
import Element.Font as Font exposing (..)
import Html.Attributes exposing (style)
import Morphir.Elm.Frontend as Frontend
import Morphir.IR as IR exposing (IR)
import Morphir.IR.Distribution as Distribution exposing (Distribution(..))
import Morphir.IR.FQName exposing (FQName)
import Morphir.IR.Name exposing (Name)
import Morphir.IR.SDK.Basics as Basics
import Morphir.IR.Type as Type exposing (Type)
import Morphir.IR.Value as Value exposing (RawValue, TypedValue, Value(..), indexedMapValue)
import Morphir.Type.Infer as Infer exposing (TypeError)
import Morphir.Visual.BoolOperatorTree as BoolOperatorTree exposing (BoolOperatorTree)
import Morphir.Visual.Common exposing (definition, nameToText)
import Morphir.Visual.Components.AritmeticExpressions as ArithmeticOperatorTree exposing (ArithmeticOperatorTree)
import Morphir.Visual.Config as Config exposing (Config)
import Morphir.Visual.Theme exposing (mediumPadding, mediumSpacing, smallPadding, smallSpacing)
import Morphir.Visual.ViewApply as ViewApply
import Morphir.Visual.ViewArithmetic as ViewArithmetic
import Morphir.Visual.ViewBoolOperatorTree as ViewBoolOperatorTree
import Morphir.Visual.ViewField as ViewField
import Morphir.Visual.ViewIfThenElse as ViewIfThenElse
import Morphir.Visual.ViewList as ViewList
import Morphir.Visual.ViewLiteral as ViewLiteral
import Morphir.Visual.ViewReference as ViewReference
import Morphir.Visual.ViewTuple as ViewTuple
import Morphir.Visual.VisualValue exposing (VisualValue)
import Morphir.Visual.XRayView as XRayView
import Morphir.Web.Theme.Light exposing (gray, silver)


viewDefinition : Config msg -> FQName -> Value.Definition () (Type ()) -> Element msg
viewDefinition config ( _, _, valueName ) valueDef =
    let
        _ =
            Debug.log "variables" config.state.variables

        definitionElem =
            definition config
                (nameToText valueName)
                (viewValue config (valueDef.body |> toVisualTypedValue))
    in
    Element.column [ mediumSpacing config.state.theme |> spacing ]
        [ definitionElem
        , if Dict.isEmpty config.state.expandedFunctions then
            Element.none

          else
            Element.column
                [ mediumSpacing config.state.theme |> spacing ]
                (config.state.expandedFunctions
                    |> Dict.toList
                    |> List.reverse
                    |> List.map
                        (\( ( _, _, localName ) as fqName, valDef ) ->
                            Element.column
                                [ smallSpacing config.state.theme |> spacing ]
                                [ definition config (nameToText localName) (viewValue config (valDef.body |> toVisualTypedValue))
                                , Element.el
                                    [ Font.bold
                                    , Border.solid
                                    , Border.rounded 4
                                    , Background.color silver
                                    , smallPadding config.state.theme |> padding
                                    , smallSpacing config.state.theme |> spacing
                                    , onClick (config.handlers.onReferenceClicked fqName True)
                                    ]
                                    (Element.text "Close")
                                ]
                        )
                )
        ]


viewValue : Config msg -> VisualValue -> Element msg
viewValue config value =
    viewValueByValueType config value


viewValueByValueType : Config msg -> VisualValue -> Element msg
viewValueByValueType config typedValue =
    let
        valueType : Type ()
        valueType =
            Value.valueAttribute typedValue |> Tuple.second
    in
    if valueType == Basics.boolType () then
        let
            boolOperatorTree : BoolOperatorTree
            boolOperatorTree =
                BoolOperatorTree.fromTypedValue typedValue
        in
        ViewBoolOperatorTree.view config (viewValueByLanguageFeature config) boolOperatorTree

    else if Basics.isNumber valueType then
        let
            arithmeticOperatorTree : ArithmeticOperatorTree
            arithmeticOperatorTree =
                ArithmeticOperatorTree.fromArithmeticTypedValue typedValue
        in
        ViewArithmetic.view config (viewValueByLanguageFeature config) arithmeticOperatorTree

    else
        viewValueByLanguageFeature config typedValue


viewValueByLanguageFeature : Config msg -> VisualValue -> Element msg
viewValueByLanguageFeature config value =
    let
        valueElem : Element msg
        valueElem =
            case value of
                Value.Literal _ literal ->
                    ViewLiteral.view config literal

                Value.Constructor _ fQName ->
                    ViewReference.view config (viewValue config) fQName

                Value.Tuple _ elems ->
                    ViewTuple.view config (viewValue config) elems

                Value.List ( index, Type.Reference _ ( [ [ "morphir" ], [ "s", "d", "k" ] ], [ [ "list" ] ], [ "list" ] ) [ itemType ] ) items ->
                    ViewList.view config (viewValue config) itemType items

                Value.Variable ( index, tpe ) name ->
                    let
                        variableValue : Maybe RawValue
                        variableValue =
                            Dict.get name config.state.variables
                    in
                    el
                        [ onMouseEnter (config.handlers.onHoverOver index variableValue)
                        , onMouseLeave (config.handlers.onHoverLeave index)
                        , Element.below
                            (if config.state.popupVariables.variableIndex == index then
                                el [ smallPadding config.state.theme |> padding ] (viewPopup config)

                             else
                                Element.none
                            )
                        , width fill
                        , center
                        ]
                        (text (nameToText name))

                Value.Reference _ fQName ->
                    ViewReference.view config (viewValue config) fQName

                Value.Field _ subjectValue fieldName ->
                    ViewField.view (viewValue config) subjectValue fieldName

                Value.Apply _ fun arg ->
                    let
                        ( function, args ) =
                            Value.uncurryApply fun arg
                    in
                    ViewApply.view config (viewValue config) function args

                Value.LetDefinition _ _ _ _ ->
                    let
                        unnest : Config msg -> VisualValue -> ( List ( Name, Element msg ), Element msg )
                        unnest conf v =
                            case v of
                                Value.LetDefinition _ defName def inVal ->
                                    let
                                        currentState =
                                            conf.state

                                        newState =
                                            { currentState
                                                | variables =
                                                    conf
                                                        |> Config.evaluate
                                                            (def
                                                                |> Value.mapDefinitionAttributes (always ()) (always ())
                                                                |> Value.definitionToValue
                                                            )
                                                        |> Result.map
                                                            (\evaluatedDefValue ->
                                                                currentState.variables
                                                                    |> Dict.insert defName evaluatedDefValue
                                                            )
                                                        |> Result.withDefault currentState.variables
                                            }

                                        ( defs, bottomIn ) =
                                            unnest { conf | state = newState } inVal
                                    in
                                    ( ( defName, viewValue conf def.body ) :: defs, bottomIn )

                                notLet ->
                                    ( [], viewValue conf notLet )

                        ( definitions, inValueElem ) =
                            unnest config value
                    in
                    Element.column
                        [ mediumSpacing config.state.theme |> spacing ]
                        [ inValueElem
                        , Element.column
                            [ mediumSpacing config.state.theme |> spacing ]
                            (definitions
                                |> List.map
                                    (\( defName, defElem ) ->
                                        Element.column
                                            [ mediumSpacing config.state.theme |> spacing ]
                                            [ definition config (nameToText defName) defElem ]
                                    )
                            )
                        ]

                Value.IfThenElse _ _ _ _ ->
                    ViewIfThenElse.view config (viewValue config) value

                other ->
                    Element.column
                        [ Background.color (rgb 1 0.6 0.6)
                        , smallPadding config.state.theme |> padding
                        , Border.rounded 6
                        ]
                        [ Element.el
                            [ smallPadding config.state.theme |> padding
                            , Font.bold
                            ]
                            (Element.text "No visual mapping found for:")
                        , Element.el
                            [ Background.color (rgb 1 1 1)
                            , smallPadding config.state.theme |> padding
                            , Border.rounded 6
                            , width fill
                            ]
                            (XRayView.viewValue XRayView.viewType (other |> Value.mapValueAttributes identity (\( _, tpe ) -> tpe)))
                        ]
    in
    valueElem


viewPopup : Config msg -> Element msg
viewPopup config =
    Element.column []
        [ case config.state.popupVariables.variableValue of
            Just value ->
                let
                    references : IR
                    references =
                        Frontend.defaultDependencies
                            |> Dict.insert
                                (Distribution.lookupPackageName config.irContext.distribution)
                                (Distribution.lookupPackageSpecification config.irContext.distribution)
                            |> IR.fromPackageSpecifications

                    typedVal : Result TypeError VisualValue
                    typedVal =
                        Infer.inferValue references value
                            |> Result.andThen
                                (\typedValue ->
                                    typedValue
                                        |> Value.mapValueAttributes identity (\( _, tpe ) -> tpe)
                                        |> toVisualTypedValue
                                        |> Ok
                                )
                in
                case typedVal of
                    Ok typedValue ->
                        el
                            [ Border.shadow
                                { offset = ( 2, 2 )
                                , size = 2
                                , blur = 2
                                , color = gray
                                }
                            , Background.color silver
                            , Font.bold
                            , Border.rounded 4
                            , Font.center
                            , mediumPadding config.state.theme |> padding
                            , htmlAttribute (style "position" "absolute")
                            , htmlAttribute (style "transition" "all 0.2s ease-in-out")
                            ]
                            (viewValue config typedValue)

                    Err error ->
                        el
                            [ Border.shadow
                                { offset = ( 2, 2 )
                                , size = 2
                                , blur = 2
                                , color = gray
                                }
                            , Background.color silver
                            , Font.bold
                            , Border.rounded 4
                            , Font.center
                            , mediumPadding config.state.theme |> padding
                            , htmlAttribute (style "position" "absolute")
                            , htmlAttribute (style "transition" "all 0.2s ease-in-out")
                            ]
                            (text (Infer.typeErrorToMessage error))

            Nothing ->
                el [] (text "")
        ]


toVisualTypedValue : TypedValue -> VisualValue
toVisualTypedValue typedValue =
    typedValue
        |> indexedMapValue Tuple.pair 0
        |> Tuple.first
