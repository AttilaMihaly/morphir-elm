module Morphir.Web.DevelopApp exposing (IRState(..), Model, Msg(..), Page(..), ServerState(..), httpMakeModel, init, main, makeURL, routeParser, scaled, subscriptions, toRoute, update, view, viewAsCard, viewBody, viewHeader, viewModuleControls, viewTitle, viewValue)

import Browser
import Browser.Navigation as Nav
import Dict exposing (Dict)
import Element exposing (Element, alignTop, centerX, centerY, column, el, fill, height, image, layout, link, minimum, none, padding, paddingXY, px, rgb, row, scrollbars, shrink, spacing, text, width, wrappedRow)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input exposing (labelHidden)
import Http
import Morphir.Correctness.Codec exposing (decodeTestSuite)
import Morphir.Correctness.Test exposing (TestCase, TestCases, TestSuite)
import Morphir.IR as IR exposing (IR)
import Morphir.IR.Distribution as Distribution exposing (Distribution(..))
import Morphir.IR.Distribution.Codec as DistributionCodec
import Morphir.IR.FQName as FQName exposing (FQName)
import Morphir.IR.Name as Name exposing (Name)
import Morphir.IR.Package as Package
import Morphir.IR.Path as Path
import Morphir.IR.QName as QName exposing (QName(..))
import Morphir.IR.Type exposing (Type)
import Morphir.IR.Value as Value exposing (RawValue, TypedValue, Value)
import Morphir.Type.Infer as Infer exposing (TypeError)
import Morphir.Value.Interpreter as Interpreter
import Morphir.Visual.Config exposing (Config, PopupScreenRecord)
import Morphir.Visual.Edit as Edit
import Morphir.Visual.Theme as Theme
import Morphir.Visual.ViewValue as ViewValue
import Morphir.Visual.VisualTypedValue exposing (VisualTypedValue, rawToVisualTypedValue)
import Morphir.Visual.XRayView as XRayView
import Morphir.Web.DevelopApp.Common exposing (scaled, viewAsCard)
import Morphir.Web.DevelopApp.ModulePage as ModulePage exposing (makeURL)
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
    , currentPage : Page
    , theme : Theme Msg
    , irState : IRState
    , serverState : ServerState
    , testSuite : TestSuite
    }


type IRState
    = IRLoading
    | IRLoaded Distribution


type ServerState
    = ServerReady
    | ServerHttpError Http.Error


type Page
    = Home
    | Module ModulePage.Model
    | Function FQName
    | NotFound


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    ( { key = key
      , currentPage = toRoute url
      , theme = Light.theme scaled
      , irState = IRLoading
      , serverState = ServerReady
      , testSuite = Dict.empty
      }
    , Cmd.batch [ httpMakeModel ]
    )



-- UPDATE


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | HttpError Http.Error
    | ServerGetIRResponse Distribution
    | ServerGetTestsResponse TestSuite
    | ExpandReference FQName Bool
    | ExpandVariable Int (Maybe RawValue)
    | ShrinkVariable Int
    | ValueFilterChanged String
    | ArgValueUpdated FQName Name RawValue
    | InvalidArgValue FQName Name String


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
            ( { model | currentPage = toRoute url }
            , Cmd.none
            )

        HttpError httpError ->
            ( { model | serverState = ServerHttpError httpError }
            , Cmd.none
            )

        ServerGetIRResponse distribution ->
            ( { model | irState = IRLoaded distribution }
            , httpTestModel distribution
            )

        ValueFilterChanged filterString ->
            case model.currentPage of
                Module moduleModel ->
                    let
                        newModuleModel =
                            { moduleModel
                                | filter = Just filterString
                            }
                    in
                    ( { model
                        | currentPage = Module newModuleModel
                      }
                    , Nav.replaceUrl model.key
                        (makeURL newModuleModel)
                    )

                _ ->
                    ( model, Cmd.none )

        ExpandReference (( _, moduleName, localName ) as fQName) isFunctionPresent ->
            if model.expandedValues |> Dict.member ( fQName, localName ) then
                case isFunctionPresent of
                    True ->
                        ( { model | expandedValues = model.expandedValues |> Dict.remove ( fQName, localName ) }, Cmd.none )

                    False ->
                        ( model, Cmd.none )

            else
                ( { model
                    | expandedValues =
                        Distribution.lookupValueDefinition (QName moduleName localName)
                            (case model.irState of
                                IRLoaded distribution ->
                                    distribution

                                _ ->
                                    Library [] Dict.empty Package.emptyDefinition
                            )
                            |> Maybe.map (\valueDef -> model.expandedValues |> Dict.insert ( fQName, localName ) valueDef)
                            |> Maybe.withDefault model.expandedValues
                  }
                , Cmd.none
                )

        ArgValueUpdated fQName argName rawValue ->
            ( { model
                | argState =
                    model.argState
                        |> Dict.update fQName
                            (\maybeArgs ->
                                case maybeArgs of
                                    Just args ->
                                        args |> Dict.insert argName rawValue |> Just

                                    Nothing ->
                                        Dict.singleton argName rawValue |> Just
                            )
              }
            , Cmd.none
            )

        InvalidArgValue fQName argName string ->
            ( model, Cmd.none )

        ExpandVariable varIndex maybeRawValue ->
            ( { model | popupVariables = { variableIndex = varIndex, variableValue = maybeRawValue } }, Cmd.none )

        ShrinkVariable varIndex ->
            ( { model | popupVariables = { variableIndex = varIndex, variableValue = Nothing } }, Cmd.none )

        ServerGetTestsResponse testSuite ->
            ( { model | testSuite = testSuite }
            , Cmd.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- ROUTE


routeParser : UrlParser.Parser (Page -> a) a
routeParser =
    UrlParser.oneOf
        [ UrlParser.map Home UrlParser.top
        , UrlParser.map Module ModulePage.routeParser
        , UrlParser.map (\fqName -> Function fqName)
            (UrlParser.s "function"
                </> (UrlParser.string
                        |> UrlParser.map
                            (\string ->
                                FQName.fromString string ":"
                            )
                    )
            )
        , UrlParser.map (always Home) UrlParser.string
        ]


toRoute : Url -> Page
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
    case model.currentPage of
        Home ->
            "Morphir - Home"

        Module moduleModel ->
            ModulePage.viewTitle moduleModel

        NotFound ->
            "Morphir - Not Found"

        Function fqName ->
            "Morphir - " ++ (fqName |> FQName.toString)


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
            case model.currentPage of
                Home ->
                    viewAsCard (text "Modules")
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

                Module moduleModel ->
                    ModulePage.viewPage
                        { expandReference = ExpandReference
                        , expandVariable = ExpandVariable
                        , shrinkVariable = ShrinkVariable
                        , argValueUpdated = ArgValueUpdated
                        , invalidArgValue = InvalidArgValue
                        }
                        ValueFilterChanged
                        distribution
                        moduleModel

                NotFound ->
                    text "Route not found"

                Function (( packagesName, moduleName, localName ) as fQName) ->
                    let
                        testCases : TestCases
                        testCases =
                            Dict.get fQName model.testSuite
                                |> Maybe.withDefault []

                        inputTypeNames : List Name
                        inputTypeNames =
                            Distribution.lookupValueSpecification packagesName moduleName localName distribution
                                |> Maybe.map
                                    (\valueSpec ->
                                        valueSpec.inputs
                                            |> List.map (\( name, tpe ) -> name)
                                    )
                                |> Maybe.withDefault []
                    in
                    Element.column [ padding 10, spacing 10 ]
                        [ el [ Font.bold, Font.size (scaled 4) ] (text "TestCases :")
                        , viewSectionWise distribution fQName testCases inputTypeNames model
                        ]


viewSectionWise : Distribution -> FQName -> TestCases -> List Name -> Model -> Element Msg
viewSectionWise distribution fQName testCases inputTypesName model =
    let
        config : Config Msg
        config =
            { irContext =
                { distribution = distribution
                , references = Interpreter.referencesForDistribution distribution
                }
            , state =
                { expandedFunctions = Dict.empty
                , variables = Dict.empty
                , popupVariables = { variableIndex = 0, variableValue = Nothing }
                , theme = Theme.fromConfig Nothing
                }
            , handlers =
                { onReferenceClicked = ExpandReference
                , onHoverOver = ExpandVariable
                , onHoverLeave = ShrinkVariable
                }
            }

        references : IR
        references =
            IR.fromDistribution distribution
    in
    List.indexedMap Tuple.pair testCases
        |> List.map
            (\( index, testcase ) ->
                column [ spacing 5, padding 5 ]
                    [ el [ Font.bold, Font.size (scaled 3) ] (text ("TestCase " ++ String.fromInt index ++ " :"))
                    , el [ Font.bold, Font.size (scaled 2), spacing 5, padding 5 ] (text "Input :")
                    , List.map2 Tuple.pair inputTypesName testcase.inputs
                        |> List.map
                            (\( name, rawValue ) ->
                                column
                                    [ spacing 5, padding 5, width fill, height fill ]
                                    [ el [ Font.bold ]
                                        (text
                                            (String.append
                                                (Name.toHumanWords name
                                                    |> String.join ""
                                                )
                                                " : "
                                            )
                                        )
                                    , viewTestCase config references rawValue
                                    ]
                            )
                        |> column [ spacing 5, padding 5 ]
                    , column [ spacing 5, padding 5 ]
                        [ el [ Font.bold, Font.size (scaled 2) ] (text "Output :")
                        , el [ spacing 5, padding 5 ] (viewTestCase config references testcase.expectedOutput)
                        ]
                    , column [ spacing 5, padding 5 ]
                        [ el [ Font.bold, Font.size (scaled 2) ] (text "Description :")
                        , el [ spacing 5, padding 5 ] (text testcase.description)
                        ]
                    , Element.column [ spacing 10, padding 10 ]
                        [ el [ Font.bold, Font.size (scaled 4) ] (text "Function :")
                        , el [ centerY, centerX ] (newFunctionView distribution fQName testcase model)
                        ]
                    ]
            )
        |> column [ spacing 5, padding 5 ]


newFunctionView : Distribution -> FQName -> TestCase -> Model -> Element Msg
newFunctionView distribution fQName testcase model =
    Distribution.lookupValueDefinition (QName.fromName (FQName.getModulePath fQName) (FQName.getLocalName fQName)) distribution
        |> Maybe.map
            (\valueDef ->
                viewValue
                    { model
                        | argState =
                            Dict.insert
                                fQName
                                (List.map2
                                    (\( name, _, tpe ) rawValue ->
                                        ( name, rawValue )
                                    )
                                    valueDef.inputTypes
                                    testcase.inputs
                                    |> Dict.fromList
                                )
                                Dict.empty
                    }
                    distribution
                    fQName
                    valueDef
            )
        |> Maybe.withDefault
            (text
                (String.join " "
                    [ "Module"
                    , [ FQName.getModulePath fQName
                            |> Path.toString Name.toTitleCase "."
                      ]
                        |> String.join "."
                    , "not found"
                    ]
                )
            )


viewTable : Config Msg -> IR -> TestCases -> Element Msg
viewTable config references testCases =
    Element.table [ centerY, centerX, width fill, height fill ]
        { data = testCases
        , columns =
            [ { header =
                    Element.column [ width fill, height fill ]
                        [ el
                            [ Border.widthEach { bottom = 2, top = 0, right = 0, left = 0 }
                            , Font.bold
                            , width fill
                            , height fill
                            ]
                            (el
                                [ centerY
                                , centerX
                                , padding 5
                                ]
                                (text "Inputs")
                            )
                        ]
                        |> el
                            [ Border.width 2
                            , Font.bold
                            ]
              , width = fill
              , view =
                    \testcase ->
                        el [ padding 5, Border.widthEach { bottom = 2, top = 0, right = 2, left = 2 }, width fill, height fill ]
                            (List.map
                                (\singleInput ->
                                    viewTestCase config
                                        references
                                        singleInput
                                )
                                testcase.inputs
                                |> row [ spacing 5, padding 5, centerX, centerY ]
                            )
              }
            , { header =
                    el
                        [ Border.widthEach { bottom = 2, top = 2, right = 2, left = 0 }
                        , padding 5
                        , Font.bold
                        , width fill
                        , height fill
                        ]
                        (el [ centerY, centerX ] (text "Outputs"))
              , width = fill
              , view =
                    \testcase ->
                        el [ Border.widthEach { bottom = 2, top = 0, right = 2, left = 0 }, width fill, height fill ]
                            (viewTestCase config references testcase.expectedOutput)
              }
            , { header =
                    el
                        [ Border.widthEach { bottom = 2, top = 2, right = 2, left = 0 }
                        , padding 5
                        , Font.bold
                        , width fill
                        , height fill
                        ]
                        (el [ centerY, centerX ] (text "Description"))
              , width = fill
              , view =
                    \testcase ->
                        el [ Border.widthEach { bottom = 2, top = 0, right = 2, left = 0 }, width fill, height fill ]
                            (el [ centerY, centerX ] (text testcase.description))
              }
            ]
        }


viewTestCase : Config Msg -> IR -> RawValue -> Element Msg
viewTestCase config references rawValue =
    case rawToVisualTypedValue references rawValue of
        Ok typedValue ->
            el [ centerX, centerY ] (ViewValue.viewValue config typedValue)

        Err error ->
            el [ centerX, centerY ] (text (Infer.typeErrorToMessage error))



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


httpTestModel : Distribution -> Cmd Msg
httpTestModel distribution =
    Http.get
        { url = "/server/morphir-tests.json"
        , expect =
            Http.expectJson
                (\response ->
                    case response of
                        Err httpError ->
                            HttpError httpError

                        Ok result ->
                            ServerGetTestsResponse result
                )
                (decodeTestSuite (IR.fromDistribution distribution))
        }
