module Morphir.Web.OpenAIRequest exposing (..)

import Browser
import Html exposing (Html, button, div, form, input, pre, text, textarea)
import Html.Attributes exposing (placeholder, value)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as Decode
import Json.Encode as Encode



-- MODEL


type alias Model =
    { userInput : String
    , apiKey : String
    , apiResponse : String
    , loading : Bool
    , error : String
    }


init : () -> ( Model, Cmd Msg )
init flags =
    ( { userInput = ""
      , apiKey = ""
      , apiResponse = ""
      , loading = False
      , error = ""
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = UpdateInput String
    | UpdateApiKey String
    | SendRequest
    | ReceiveResponse (Result Http.Error String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateInput input ->
            ( { model | userInput = input }
            , Cmd.none
            )

        UpdateApiKey key ->
            ( { model | apiKey = key }
            , Cmd.none
            )

        SendRequest ->
            ( { model | loading = True, apiResponse = "", error = "" }
            , fetchAPI model.userInput model.apiKey ReceiveResponse
            )

        ReceiveResponse (Ok response) ->
            ( { model | apiResponse = response, loading = False }
            , Cmd.none
            )

        ReceiveResponse (Err _) ->
            ( { model | error = "Failed to fetch data", loading = False }
            , Cmd.none
            )



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ form []
            [ input
                [ placeholder "Enter your API key..."
                , onInput UpdateApiKey
                , value model.apiKey
                ]
                []
            , textarea
                [ placeholder "Enter your prompt here..."
                , onInput UpdateInput
                , value model.userInput
                ]
                []
            , div
                [ onClick SendRequest ]
                [ text "Send" ]
            ]
        , div []
            [ if model.loading then
                text "Loading..."

              else
                pre [] [ text model.apiResponse ]
            ]
        , if model.error /= "" then
            div [] [ text model.error ]

          else
            text ""
        ]



-- HTTP


fetchAPI : String -> String -> (Result Http.Error String -> msg) -> Cmd msg
fetchAPI prompt apiKey onReceiveResponse =
    let
        url : String
        url =
            "https://api.openai.com/v1/chat/completions"

        instructions : List String
        instructions =
            [ "You are a helpful assistant that generates Elm code."
            , "Include only the code in the response."
            , "Use plain text."
            , "Do not use markdown."
            , "Include description in Elm doc format."
            , "Do not include module declaration."
            ]

        body : Encode.Value
        body =
            Encode.object
                [ ( "model", Encode.string "gpt-4o" )
                , ( "messages"
                  , Encode.list identity
                        [ Encode.object
                            [ ( "role", Encode.string "system" )
                            , ( "content", Encode.string (String.join " " instructions) )
                            ]
                        , Encode.object
                            [ ( "role", Encode.string "user" )
                            , ( "content", Encode.string prompt )
                            ]
                        ]
                  )
                , ( "max_tokens", Encode.int 300 )
                , ( "temperature", Encode.float 0.7 )
                ]

        request : Cmd msg
        request =
            Http.request
                { method = "POST"
                , headers =
                    [ Http.header "Authorization" ("Bearer " ++ apiKey)
                    ]
                , url = url
                , body = Http.jsonBody body
                , expect = Http.expectJson onReceiveResponse responseDecoder
                , timeout = Nothing
                , tracker = Nothing
                }
    in
    request


responseDecoder : Decode.Decoder String
responseDecoder =
    Decode.map String.concat
        (Decode.field
            "choices"
            (Decode.list
                (Decode.field "message"
                    (Decode.field "content" Decode.string)
                )
            )
        )



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }
