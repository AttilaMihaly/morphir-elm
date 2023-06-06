module Morphir.Web.EditMorphir exposing (..)

import Browser
import Dict exposing (Dict)
import Element exposing (Element, alignRight, column, el, fill, height, layout, none, padding, paddingEach, paddingXY, paragraph, px, rgb, row, scrollbars, shrink, spacing, table, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import FontAwesome.Styles as Icon
import Html exposing (Html)
import Http
import Json.Encode as Encode
import Morphir.Compiler as Compiler
import Morphir.Elm.Frontend as Frontend exposing (SourceFile)
import Morphir.IR as IR exposing (IR)
import Morphir.IR.Distribution as Distribution exposing (Distribution(..))
import Morphir.IR.Distribution.Codec as DistributionCodec
import Morphir.IR.FQName as FQName exposing (FQName, fqn)
import Morphir.IR.Module as Module exposing (ModuleName)
import Morphir.IR.Name as Name exposing (Name)
import Morphir.IR.Package as Package exposing (PackageName)
import Morphir.IR.SDK as SDK
import Morphir.IR.SDK.Basics as Basics
import Morphir.IR.Type as Type exposing (Type)
import Morphir.IR.Type.Codec as TypeCodec
import Morphir.IR.Value as Value
import Morphir.IR.Value.Codec as ValueCodec
import Morphir.Type.Constraint exposing (Constraint(..))
import Morphir.Type.ConstraintSet as ConstraintSet exposing (ConstraintSet)
import Morphir.Type.Count as Count
import Morphir.Type.Infer as Infer
import Morphir.Type.MetaType exposing (MetaType(..))
import Morphir.Type.Solve as Solve exposing (SolutionMap)
import Morphir.Visual.Common exposing (nameToText, pathToUrl)
import Morphir.Visual.Components.Card as Card
import Morphir.Visual.Components.FieldList as FieldList
import Morphir.Visual.Components.Picklist as Picklist
import Morphir.Visual.Components.TypeInferenceView as TypeInferenceView
import Morphir.Visual.Config exposing (Config, DrillDownFunctions(..))
import Morphir.Visual.Theme as Theme exposing (Theme)
import Morphir.Visual.ValueEditor as ValueEditor
import Morphir.Visual.ViewType as ViewType
import Morphir.Visual.ViewValue as ViewValue
import Morphir.Visual.XRayView as XRayView
import Morphir.Web.SourceEditor as SourceEditor



-- MAIN


type alias Flags =
    {}


main =
    Browser.document { init = init, update = update, view = view, subscriptions = subscriptions }



-- MODEL


type Model
    = Loading
    | Loaded LoadedModel


type alias LoadedModel =
    { distribution : Distribution
    , pickField : Picklist.State (Type.Field ())
    , pickFun : Picklist.State ( FQName, Value.Specification () )
    , editValues : List ( Type (), ValueEditor.EditorState )
    , selectableFields : List (Type.Field ())
    , selectableFuns : List ( FQName, Value.Specification () )
    }


theme : Theme
theme =
    Theme.fromConfig Nothing


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( Loading, httpMakeModel )



-- UPDATE


type Msg
    = HttpError String Http.Error
    | ServerGetIRResponse Distribution
    | PickFieldChanged (Picklist.State (Type.Field ()))
    | PickFunChanged (Picklist.State ( FQName, Value.Specification () ))
    | ValueChanged Int ValueEditor.EditorState
    | DoNothing


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( model, msg ) of
        ( Loading, ServerGetIRResponse distro ) ->
            let
                loadedModel : LoadedModel
                loadedModel =
                    { distribution = distro
                    , pickField = Picklist.init Nothing
                    , pickFun = Picklist.init Nothing
                    , editValues = []
                    , selectableFields = inputFields distro (FQName.fromString "Regulation:US.FR2052A.DataTables.Inflows.Assets:Assets" ":")
                    , selectableFuns = []
                    }
            in
            ( Loaded loadedModel, Cmd.none )

        ( Loaded loadedModel, PickFieldChanged picklistState ) ->
            let
                selectableFuns =
                    case picklistState |> Picklist.getSelectedTag of
                        Just selectedField ->
                            applicableFunctions loadedModel.distribution selectedField.tpe

                        Nothing ->
                            []

                newLoadedModel =
                    { loadedModel
                        | pickField = picklistState
                        , pickFun = Picklist.init Nothing
                        , editValues = []
                        , selectableFuns = selectableFuns
                    }
            in
            ( Loaded newLoadedModel, Cmd.none )

        ( Loaded loadedModel, PickFunChanged pickFunState ) ->
            let
                editValues =
                    case pickFunState |> Picklist.getSelectedTag of
                        Just ( _, selectedFun ) ->
                            selectedFun.inputs
                                |> List.drop 1
                                |> List.map
                                    (\( _, argType ) ->
                                        ( argType, ValueEditor.initEditorState (IR.fromDistribution loadedModel.distribution) argType Nothing )
                                    )

                        Nothing ->
                            []

                newLoadedModel =
                    { loadedModel
                        | pickFun = pickFunState
                        , editValues = editValues
                    }
            in
            ( Loaded newLoadedModel, Cmd.none )

        ( Loaded loadedModel, ValueChanged index valueEditorState ) ->
            ( Loaded
                { loadedModel
                    | editValues = loadedModel.editValues |> set index (\( t, _ ) -> ( t, valueEditorState ))
                }
            , Cmd.none
            )

        _ ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- HTTP


httpMakeModel : Cmd Msg
httpMakeModel =
    Http.get
        { url = "/server/morphir-ir.json"
        , expect =
            Http.expectJson
                (\response ->
                    case response of
                        Err httpError ->
                            HttpError "We encountered an issue while loading the IR" httpError

                        Ok result ->
                            ServerGetIRResponse result
                )
                DistributionCodec.decodeVersionedDistribution
        }



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "Morphir - Home"
    , body =
        [ Icon.css
        , layout
            [ width fill
            , height fill
            , Font.family
                [ Font.external
                    { name = "Poppins"
                    , url = "https://fonts.googleapis.com/css2?family=Poppins:wght@400&display=swap"
                    }
                , Font.sansSerif
                ]
            , Font.size theme.fontSize
            ]
            (case model of
                Loading ->
                    text "Loading ..."

                Loaded loadedModel ->
                    viewLoaded loadedModel
            )
        ]
    }


viewLoaded : LoadedModel -> Element Msg
viewLoaded model =
    let
        typeInfo : Value.Specification () -> Element Msg
        typeInfo vSpec =
            (vSpec.inputs
                |> List.map Tuple.second
            )
                |> List.map Type.toString
                |> String.join " -> "
                |> text
    in
    column [ spacing 10 ]
        [ row
            [ padding 5
            , spacing 5
            ]
            [ Picklist.view theme
                -- this section is for wiring the component into your application,
                -- check out the Config type docs for further details
                { state = model.pickField
                , onStateChange = PickFieldChanged
                }
                -- this is where you specify the selectable values
                -- each entry is a tuple where the first element is the "tag" that represents the selection
                (model.selectableFields
                    |> List.map
                        (\field ->
                            ( field, field.name |> Name.toHumanWords |> String.join " " |> text )
                        )
                )
            , Picklist.view theme
                -- this section is for wiring the component into your application,
                -- check out the Config type docs for further details
                { state = model.pickFun
                , onStateChange = PickFunChanged
                }
                -- this is where you specify the selectable values
                -- each entry is a tuple where the first element is the "tag" that represents the selection
                (model.selectableFuns
                    |> List.map
                        (\( ( _, _, valueName ) as fqn, valueSpec ) ->
                            ( ( fqn, valueSpec ), text (valueName |> Name.toHumanWords |> String.join " ") )
                        )
                )
            , model.editValues
                |> List.indexedMap
                    (\index ( argType, valueEditorState ) ->
                        ValueEditor.view theme
                            (IR.fromDistribution model.distribution)
                            argType
                            (ValueChanged index)
                            valueEditorState
                    )
                |> row [ spacing 5 ]
            ]
        , viewIR model
        ]


viewIR : LoadedModel -> Element Msg
viewIR model =
    none


inputFields : Distribution -> FQName -> List (Type.Field ())
inputFields distro ( packageName, moduleName, localName ) =
    case distro |> Distribution.lookupTypeSpecification packageName moduleName localName of
        Just (Type.TypeAliasSpecification _ (Type.Record _ fields)) ->
            fields

        _ ->
            []


applicableFunctions : Distribution -> Type () -> List ( FQName, Value.Specification () )
applicableFunctions ((Library packageName dependencies packageDef) as distro) inputType =
    let
        ir : IR
        ir =
            IR.fromDistribution distro

        resolvedInputType : Type ()
        resolvedInputType =
            ir |> IR.resolveType inputType

        collectValueSpecs : (Value.Specification () -> Maybe (Value.Specification ())) -> PackageName -> Package.Specification () -> List ( FQName, Value.Specification () )
        collectValueSpecs match pName pSpec =
            pSpec.modules
                |> Dict.toList
                |> List.concatMap
                    (\( moduleName, moduleSpec ) ->
                        moduleSpec.values
                            |> Dict.toList
                            |> List.filterMap
                                (\( valueName, documentedValueSpec ) ->
                                    case match documentedValueSpec.value of
                                        Just updatedValueSpec ->
                                            Just ( ( pName, moduleName, valueName ), updatedValueSpec )

                                        Nothing ->
                                            Nothing
                                )
                    )

        matchInputType : Value.Specification () -> Maybe (Value.Specification ())
        matchInputType valueSpec =
            if (ir |> IR.resolveType valueSpec.output) == Basics.boolType () then
                applyConcreteArgumentType ir 0 resolvedInputType valueSpec

            else
                Nothing

        sdkValues : List ( FQName, Value.Specification () )
        sdkValues =
            collectValueSpecs
                matchInputType
                SDK.packageName
                SDK.packageSpec

        packageValues : List ( FQName, Value.Specification () )
        packageValues =
            collectValueSpecs
                matchInputType
                packageName
                (packageDef |> Package.definitionToSpecification)
    in
    packageValues ++ sdkValues


applyConcreteArgumentType : IR -> Int -> Type () -> Value.Specification () -> Maybe (Value.Specification ())
applyConcreteArgumentType ir argIndex concreteArgType valueSpec =
    let
        typeMatchesVariable : Type () -> Name -> Bool
        typeMatchesVariable tpe varName =
            case varName of
                "number" :: _ ->
                    False

                "comparable" :: _ ->
                    False

                "appendable" :: _ ->
                    False

                "comp" :: "append" :: _ ->
                    False

                _ ->
                    True
    in
    case get argIndex valueSpec.inputs of
        Just ( _, aType ) ->
            case ir |> IR.resolveType aType of
                Type.Variable _ varName ->
                    if typeMatchesVariable concreteArgType varName then
                        Just
                            { valueSpec
                                | inputs =
                                    valueSpec.inputs
                                        |> List.map
                                            (\( argName, argType ) ->
                                                ( argName, argType |> Type.substituteTypeVariables (Dict.singleton varName concreteArgType) )
                                            )
                            }

                    else
                        Nothing

                otherType ->
                    if otherType == concreteArgType then
                        Just valueSpec

                    else
                        Nothing

        Nothing ->
            Nothing


get : Int -> List a -> Maybe a
get index list =
    case list of
        head :: tail ->
            if index == 0 then
                Just head

            else
                get (index - 1) tail

        [] ->
            Nothing


set : Int -> (a -> a) -> List a -> List a
set index value list =
    list
        |> List.indexedMap
            (\i item ->
                if i == index then
                    value item

                else
                    item
            )
