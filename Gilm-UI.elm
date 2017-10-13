module Gilm exposing (..)


import List
import Html
import Svg exposing (..)
import Svg.Attributes exposing (..)


main =
    Html.program
        { init = ( "Moin", Cmd.none )
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model = String

type Msg = Nothing

view : Model -> Html.Html Msg
view model =
    Html.body []
        [ Html.h1 [] [text ("Model: " ++ model)]
        , svg [ version "1.1", x "0", y "0", viewBox "0 0 323.141 322.95"]
            <| List.append
                [ polygon [ fill "#F0AD00", points "161.649,152.782 231.514,82.916 91.783,82.916" ] []
                , polygon [ fill "#7FD13B", points "8.867,0 79.241,70.375 232.213,70.375 161.838,0" ] []
                , rect
                    [ fill "#7FD13B", x "192.99", y "107.392", width "107.676", height "108.167"
                    , transform "matrix(0.7071 0.7071 -0.7071 0.7071 186.4727 -127.2386)"
                    ]
                    []
                , polygon [ fill "#60B5CC", points "323.298,143.724 323.298,0 179.573,0" ] []
                , polygon [ fill "#5A6378", points "152.781,161.649 0,8.868 0,314.432" ] []
                , polygon [ fill "#F0AD00", points "255.522,246.655 323.298,314.432 323.298,178.879" ] []
                , polygon [ fill "#60B5CC", points "161.649,170.517 8.869,323.298 314.43,323.298" ] []
                ]
                (graph model)
        ]


graph : Model -> List (Svg Msg)
graph model =
    [ rect [fill "#000000", x "50", y "50", width "100", height "25"] []
    ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Nothing ->
            ( "nix", Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
