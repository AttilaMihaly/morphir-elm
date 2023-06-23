module Morphir.Web.PipelineEditorApp exposing (..)

-- Press buttons to increment and decrement a counter.
--
-- Read how it works:
--   https://guide.elm-lang.org/architecture/buttons.html
--

import Browser
import Element exposing (el, fill, height, layout, padding, width)
import Element.Font as Font
import Html exposing (Html)
import Morphir.Visual.Components.PipelineEditor as PipelineEditor



-- MAIN


main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }



-- MODEL


type alias Model =
    { pipeline : PipelineEditor.Pipeline String
    }


init : Model
init =
    { pipeline =
        PipelineEditor.ProcessingNode "Select"
            [ PipelineEditor.ProcessingNode "Filter"
                [ PipelineEditor.ProcessingNode "Join 1"
                    [ PipelineEditor.SourceNode "Source A"
                    , PipelineEditor.SourceNode "Source B"
                    ]
                ]
            , PipelineEditor.ProcessingNode "Join 2"
                [ PipelineEditor.SourceNode "Source D"
                , PipelineEditor.SourceNode "Source E"
                ]
            ]
    }



-- UPDATE


type Msg
    = Increment
    | Decrement


update : Msg -> Model -> Model
update msg model =
    case msg of
        Increment ->
            model

        Decrement ->
            model



-- VIEW


view : Model -> Html Msg
view model =
    layout
        [ width fill
        , height fill
        , Font.family
            [ Font.external
                { name = "Source Code Pro"
                , url = "https://fonts.googleapis.com/css2?family=Source+Code+Pro&display=swap"
                }
            , Font.monospace
            ]
        , Font.size 16
        ]
        (el [ padding 20 ]
            (PipelineEditor.viewPipeline Element.text model.pipeline)
        )
