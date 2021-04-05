module Morphir.Web.DevelopApp exposing (IRState(..), Model, Msg(..), Route(..), ServerState(..), ViewType(..), httpMakeModel, init, main, makeURL, noBorderWidth, routeParser, scaled, subscriptions, toRoute, update, view, viewAsCard, viewBody, viewHeader, viewModuleControls, viewTitle, viewTypeFromString, viewValue)

import Browser
import Browser.Navigation as Nav
import Dict exposing (Dict)
import Element exposing (Color, Element, alignTop, column, el, fill, height, image, layout, link, minimum, none, padding, paddingXY, px, rgb, row, shrink, spacing, table, text, width, wrappedRow)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input exposing (button, labelHidden)
import Http
import Morphir.Correctness.Test as Test
import Morphir.IR.Distribution exposing (Distribution(..))
import Morphir.IR.Distribution.Codec as DistributionCodec
import Morphir.IR.FQName exposing (FQName)
import Morphir.IR.Name as Name exposing (Name)
import Morphir.IR.Type exposing (Type)
import Morphir.IR.Value as Value exposing (RawValue, TypedValue, Value)
import Morphir.Value.Interpreter as Interpreter
import Morphir.Visual.Common exposing (nameToText)
import Morphir.Visual.Config exposing (Config, PopupScreenRecord)
import Morphir.Visual.TestScenarioEditor as TestEditor
import Morphir.Visual.Theme as Theme
import Morphir.Visual.ViewValue as ViewValue
import Morphir.Visual.XRayView as XRayView
import Morphir.Web.Theme exposing (Theme)
import Morphir.Web.Theme.Light as Light
import Url exposing (Url)
import Url.Parser as UrlParser exposing ((</>), (<?>))
import Url.Parser.Query as Query



-- MAIN


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }



-- MODEL


type alias Model =
    { key : Nav.Key
    , route : Route
    , theme : Theme Msg
    , irState : IRState
    , serverState : ServerState
    , inputStates : Dict FQName (Dict Name RawValue)
    , outputStates : Dict FQName RawValue
    , scenarioStates : Dict FQName (List Test.Scenario)
    , expandedValues : Dict ( FQName, Name ) (Value.Definition () (Type ()))
    }


type IRState
    = IRLoading
    | IRLoaded Distribution


type ServerState
    = ServerReady
    | ServerHttpError Http.Error


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    ( { key = key
      , route = toRoute url
      , theme = Light.theme scaled
      , irState = IRLoading
      , serverState = ServerReady
      , inputStates = Dict.empty
      , outputStates = Dict.empty
      , scenarioStates = Dict.empty
      , expandedValues = Dict.empty
      }
    , httpMakeModel
    )



-- UPDATE


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | HttpError Http.Error
    | ServerGetIRResponse Distribution
    | ExpandReference FQName Bool
    | ExpandVariable Int (Maybe RawValue)
    | ShrinkVariable Int
    | ValueFilterChanged String
    | InputValueUpdated FQName Name (Maybe RawValue)
    | InvalidInputValue FQName Name String
    | OutputValueUpdated FQName (Maybe RawValue)
    | AddScenario FQName


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg |> Debug.log "msg" of
        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            ( { model | route = toRoute url }
            , Cmd.none
            )

        HttpError httpError ->
            ( { model | serverState = ServerHttpError httpError }
            , Cmd.none
            )

        ServerGetIRResponse distribution ->
            ( { model | irState = IRLoaded distribution }
            , Cmd.none
            )

        ValueFilterChanged filterString ->
            let
                newRoute =
                    case model.route of
                        Module moduleName _ viewType ->
                            Module moduleName (Just filterString) viewType

                        _ ->
                            model.route

                cmd =
                    case model.route of
                        Module moduleName filter viewType ->
                            Nav.replaceUrl model.key
                                (makeURL moduleName (Just filterString) viewType)

                        _ ->
                            Cmd.none
            in
            ( { model | route = newRoute }
            , cmd
            )

        ExpandReference fqn isFunctionPresent ->
            ( model
            , Cmd.none
            )

        InputValueUpdated fQName argName newValue ->
            ( { model
                | inputStates =
                    model.inputStates
                        |> Dict.update fQName
                            (\maybeArgs ->
                                case maybeArgs of
                                    Just args ->
                                        args |> Dict.update argName (always newValue) |> Just

                                    Nothing ->
                                        case newValue of
                                            Nothing ->
                                                Nothing

                                            Just rawValue ->
                                                Dict.singleton argName rawValue |> Just
                            )
              }
            , Cmd.none
            )

        OutputValueUpdated fQName newValue ->
            ( { model
                | outputStates =
                    model.outputStates
                        |> Dict.update fQName (always newValue)
              }
            , Cmd.none
            )

        InvalidInputValue fQName argName message ->
            ( model, Cmd.none )

        ExpandVariable varIndex maybeRawValue ->
            ( model, Cmd.none )

        ShrinkVariable varIndex ->
            ( model, Cmd.none )

        AddScenario fQName ->
            ( { model
                | scenarioStates =
                    model.scenarioStates
                        |> Dict.update fQName
                            (\currentScenarios ->
                                let
                                    newScenario : Maybe Test.Scenario
                                    newScenario =
                                        Maybe.map2
                                            (\inputStates outputState ->
                                                { description = Nothing
                                                , inputs = inputStates
                                                , expectedOutput = outputState
                                                }
                                            )
                                            (model.inputStates |> Dict.get fQName)
                                            (model.outputStates |> Dict.get fQName)
                                in
                                case currentScenarios of
                                    Just scenarios ->
                                        newScenario
                                            |> Maybe.map (\new -> scenarios ++ [ new ])
                                            |> Maybe.withDefault scenarios
                                            |> Just

                                    Nothing ->
                                        newScenario
                                            |> Maybe.map List.singleton
                            )
              }
            , Cmd.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- ROUTE


type Route
    = Home
    | Module (List String) (Maybe String) ViewType
    | NotFound


type ViewType
    = XRayView
    | InsightView


viewTypeFromString : String -> ViewType
viewTypeFromString string =
    case string of
        "insight" ->
            InsightView

        _ ->
            XRayView


routeParser : UrlParser.Parser (Route -> a) a
routeParser =
    UrlParser.oneOf
        [ UrlParser.map Home UrlParser.top
        , UrlParser.map
            (\moduleName filter viewType ->
                Module moduleName
                    (filter
                        |> Maybe.map
                            (\filterString ->
                                if String.endsWith "*" filterString then
                                    filterString |> String.dropRight 1

                                else
                                    filterString
                            )
                    )
                    viewType
            )
            (UrlParser.s "module"
                </> (UrlParser.string |> UrlParser.map (String.split "."))
                <?> Query.string "filter"
                <?> (Query.string "view" |> Query.map (Maybe.map viewTypeFromString >> Maybe.withDefault InsightView))
            )
        , UrlParser.map (always Home) UrlParser.string
        ]


toRoute : Url -> Route
toRoute url =
    UrlParser.parse routeParser url
        |> Maybe.withDefault NotFound



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = viewTitle model
    , body =
        [ layout
            [ Font.family
                [ Font.external
                    { name = "Poppins"
                    , url = "https://fonts.googleapis.com/css2?family=Poppins:wght@300&display=swap"
                    }
                , Font.sansSerif
                ]
            , Font.size (scaled 2)
            , width fill
            ]
            (column
                [ width fill
                ]
                [ viewHeader model
                , case model.serverState of
                    ServerReady ->
                        none

                    ServerHttpError error ->
                        viewServerError error
                , el [ padding 5, width fill ] (viewBody model)
                ]
            )
        ]
    }


viewTitle : Model -> String
viewTitle model =
    case model.route of
        Home ->
            "Morphir - Home"

        Module moduleName _ _ ->
            "Morphir - " ++ (moduleName |> String.join " / ")

        NotFound ->
            "Morphir - Not Found"


viewHeader : Model -> Element Msg
viewHeader model =
    column
        [ width fill
        , Background.color model.theme.highlightColor
        ]
        [ row
            [ width fill
            ]
            [ row
                [ width fill
                ]
                [ el [ padding 6 ]
                    (image
                        [ height (px 40)
                        ]
                        { src = "/assets/2020_Morphir_Logo_Icon_WHT.svg"
                        , description = "Morphir Logo"
                        }
                    )
                , el [ paddingXY 10 0 ]
                    (model.theme.heading 1 "Morphir Web")
                ]
            ]
        ]


viewServerError : Http.Error -> Element msg
viewServerError error =
    let
        message : String
        message =
            case error of
                Http.BadUrl url ->
                    "An invalid URL was provided: " ++ url

                Http.Timeout ->
                    "Request timed out"

                Http.NetworkError ->
                    "Network error"

                Http.BadStatus code ->
                    "Server returned an error: " ++ String.fromInt code

                Http.BadBody body ->
                    "Unexpected response body: " ++ body
    in
    el
        [ width fill
        , paddingXY 20 10
        , Background.color (rgb 1 0.5 0.5)
        , Font.color (rgb 1 1 1)
        ]
        (text message)


viewBody : Model -> Element Msg
viewBody model =
    case model.irState of
        IRLoading ->
            text "Loading the IR ..."

        IRLoaded ((Library packageName _ packageDef) as distribution) ->
            case model.route of
                Home ->
                    viewAsCard (rgb 0.9 0.9 0.9)
                        (text "Modules")
                        (column
                            [ padding 10
                            , spacing 10
                            ]
                            (packageDef.modules
                                |> Dict.toList
                                |> List.map
                                    (\( moduleName, accessControlledModuleDef ) ->
                                        link []
                                            { url =
                                                "/module/" ++ (moduleName |> List.map Name.toTitleCase |> String.join ".")
                                            , label =
                                                moduleName
                                                    |> List.map (Name.toHumanWords >> String.join " ")
                                                    |> String.join " / "
                                                    |> text
                                            }
                                    )
                            )
                        )

                Module moduleNameString filterString viewType ->
                    let
                        moduleName =
                            moduleNameString |> List.map Name.fromString
                    in
                    case packageDef.modules |> Dict.get moduleName of
                        Just accessControlledModuleDef ->
                            column
                                [ width fill
                                , spacing (scaled 4)
                                ]
                                [ viewModuleControls moduleNameString filterString viewType
                                , wrappedRow [ spacing (scaled 4) ]
                                    (accessControlledModuleDef.value.values
                                        |> Dict.toList
                                        |> List.filterMap
                                            (\( valueName, accessControlledValueDef ) ->
                                                let
                                                    matchesFilter =
                                                        case filterString of
                                                            Just filter ->
                                                                String.contains
                                                                    (filter |> String.toLower)
                                                                    (valueName
                                                                        |> Name.toHumanWords
                                                                        |> List.map String.toLower
                                                                        |> String.join " "
                                                                    )

                                                            Nothing ->
                                                                True

                                                    valueFQName =
                                                        ( packageName, moduleName, valueName )
                                                in
                                                if matchesFilter then
                                                    Just
                                                        (el [ alignTop ]
                                                            (viewAsCard (rgb 0.9 0.9 0.9)
                                                                (column [ spacing 5 ]
                                                                    [ valueName
                                                                        |> Name.toHumanWords
                                                                        |> String.join " "
                                                                        |> text

                                                                    --, viewArgumentEditors model valueFQName accessControlledValueDef.value
                                                                    , viewTestScenarios model valueFQName accessControlledValueDef.value
                                                                    ]
                                                                )
                                                                (case viewType of
                                                                    InsightView ->
                                                                        viewValue
                                                                            model
                                                                            distribution
                                                                            valueFQName
                                                                            accessControlledValueDef.value

                                                                    XRayView ->
                                                                        XRayView.viewValueDefinition XRayView.viewType accessControlledValueDef
                                                                )
                                                            )
                                                        )

                                                else
                                                    Nothing
                                            )
                                    )
                                ]

                        Nothing ->
                            text (String.join " " [ "Module", moduleNameString |> String.join ".", "not found" ])

                NotFound ->
                    text "Route not found"


scaled : Int -> Int
scaled =
    Element.modular 10 1.25 >> round



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
                            HttpError httpError

                        Ok result ->
                            ServerGetIRResponse result
                )
                DistributionCodec.decodeVersionedDistribution
        }


viewModuleControls : List String -> Maybe String -> ViewType -> Element Msg
viewModuleControls moduleName filterString viewType =
    let
        viewTypeBackground expectedType =
            if viewType == expectedType then
                Background.color (rgb 0.8 0.8 0.8)

            else
                Background.color (rgb 1 1 1)
    in
    row
        [ width fill
        , spacing (scaled 2)
        , height shrink
        ]
        [ Input.text
            [ paddingXY 10 4
            , Border.width 1
            , Border.rounded 10
            ]
            { onChange = ValueFilterChanged
            , text = filterString |> Maybe.withDefault ""
            , placeholder = Just (Input.placeholder [] (text "start typing to filter values ..."))
            , label = labelHidden "filter values"
            }
        , el []
            (row [ spacing 5 ]
                [ link [ paddingXY 6 4, Border.rounded 3, viewTypeBackground XRayView ]
                    { url = makeURL moduleName filterString XRayView
                    , label = text "x-ray"
                    }
                , text "|"
                , link [ paddingXY 6 4, Border.rounded 3, viewTypeBackground InsightView ]
                    { url = makeURL moduleName filterString InsightView
                    , label = text "insight"
                    }
                ]
            )
        ]


makeURL : List String -> Maybe String -> ViewType -> String
makeURL moduleName filterString viewType =
    String.concat
        [ "/module/"
        , moduleName |> String.join "."
        , "?filter="
        , filterString |> Maybe.withDefault ""
        , "&view="
        , case viewType of
            InsightView ->
                "insight"

            _ ->
                "raw"
        ]


viewValue : Model -> Distribution -> FQName -> Value.Definition () (Type ()) -> Element Msg
viewValue model distribution valueFQName valueDef =
    let
        popupScreen : PopupScreenRecord
        popupScreen =
            { variableIndex = 0
            , variableValue = Nothing
            }

        validArgValues : Dict Name (Value () ())
        validArgValues =
            model.inputStates
                |> Dict.get valueFQName
                |> Maybe.withDefault Dict.empty

        config : Config Msg
        config =
            { irContext =
                { distribution = distribution
                , references = Interpreter.referencesForDistribution distribution
                }
            , state =
                { expandedFunctions = Dict.empty
                , variables = validArgValues
                , popupVariables = popupScreen
                , theme = Theme.fromConfig Nothing
                }
            , handlers =
                { onReferenceClicked = ExpandReference
                , onHoverOver = ExpandVariable
                , onHoverLeave = ShrinkVariable
                }
            }
    in
    ViewValue.viewDefinition config valueFQName valueDef


viewTestScenarios : Model -> FQName -> Value.Definition () (Type ()) -> Element Msg
viewTestScenarios model valueFQName valueDef =
    viewAsCard (rgb 0.9 1.0 1.0)
        (text "Test Scenarios")
        (column [ spacing 5 ]
            [ table []
                { data =
                    model.scenarioStates
                        |> Dict.get valueFQName
                        |> Maybe.withDefault []
                , columns =
                    valueDef.inputTypes
                        |> List.map
                            (\( inputName, _, inputType ) ->
                                { header = text (nameToText inputName)
                                , width = fill
                                , view =
                                    \scenario ->
                                        scenario.inputs
                                            |> Dict.get inputName
                                            |> Maybe.map (Debug.toString >> text)
                                            |> Maybe.withDefault (text "n/a")
                                }
                            )
                }
            , viewAsCard (rgb 0.9 1.0 0.9)
                (text "Current Scenario")
                (TestEditor.view
                    { inputStates = model.inputStates |> Dict.get valueFQName |> Maybe.withDefault Dict.empty
                    , expectedOutputState = model.outputStates |> Dict.get valueFQName
                    , onInputUpdated = InputValueUpdated valueFQName
                    , onInvalidInput = InvalidInputValue valueFQName
                    , onOutputUpdated = OutputValueUpdated valueFQName
                    , onInvalidOutput = InvalidInputValue valueFQName []
                    }
                    valueDef
                )
            , button []
                { onPress = Just (AddScenario valueFQName)
                , label = text "Add scenario"
                }
            ]
        )


viewAsCard : Color -> Element msg -> Element msg -> Element msg
viewAsCard frameColor header content =
    let
        white =
            rgb 1 1 1
    in
    column
        [ Background.color frameColor
        , Border.rounded 3
        , height fill
        , width fill
        , padding 5
        , spacing 5
        ]
        [ el
            [ width fill
            , height fill
            , padding 2
            , Font.size (scaled 2)
            ]
            header
        , el
            [ Background.color white
            , Border.rounded 3
            , padding 5
            , height fill
            , width fill
            ]
            content
        ]


noBorderWidth =
    { top = 0
    , right = 0
    , bottom = 0
    , left = 0
    }
