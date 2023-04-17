module Morphir.Visual.Components.SectionComponent exposing (..)

import Element exposing (Element, column, el, fill, height, none, padding, pointer, row, spacing, text, width)
import Element.Background as Background
import Element.Events exposing (onClick)
import Element.Font as Font
import Element.Input
import Morphir.Visual.Theme as Theme exposing (Theme, mediumSpacing, smallSpacing)


type alias Config msg =
    { title : String
    , content : Element msg
    , onToggle : msg
    , isOpen : Bool
    }


view : Theme -> Config msg -> Element msg
view theme config =
    let
        header : Element msg
        header =
            let
                icon : Element msg
                icon =
                    if config.isOpen then
                        Element.map never theme.icons.opened

                    else
                        Element.map never theme.icons.closed
            in
            Element.Input.button
                []
                { onPress = Just config.onToggle
                , label =
                    row
                        [ width fill
                        , Background.color theme.colors.lightest
                        , Font.bold
                        , smallSpacing theme |> spacing
                        , pointer
                        ]
                        [ el [ padding (Theme.smallPadding theme) ] icon, el [ Font.size theme.fontSize ] (text config.title) ]
                }
    in
    column [ width fill, height fill, Background.color theme.colors.lightest, mediumSpacing theme |> spacing ]
        [ header
        , if config.isOpen then
            config.content

          else
            none
        ]
