module Cell exposing (..)

import Html exposing (Html, div, section, figure)
import Html.Attributes exposing (style, id, class)
import Html.Events exposing (onClick)

type State =
    Alive | Dead


render : State -> Html a
render state =
    let
        lightClass =
            case state of
                Alive ->
                    ""

                Dead ->
                    "flipped"

        divStyle =
            style
                [ ( "height", "95%" )
                , ( "width", "95%" )
                , ( "border-radius", "15px" )
                ]
    in
        section [ class "container" ]
            [ div
                [ id "light", class lightClass ]
                [ figure [ class "on" ] []
                , figure [ class "off" ] []
                ]
            ]
