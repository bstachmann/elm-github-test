module DagManipulationUI exposing (..)

import Bootstrap.Grid as Grid
import Dag exposing (Dag, empty, node)
import DagRenderer exposing (StreamLayout, empty, flowGraphWithHeader, toFlowLayout)
import Html exposing (Html, a, div, h5, text)
import Html.Attributes exposing (attribute, class, href, id)
import List exposing (indexedMap)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { dag : Dag String String
    , layouts : List (Transformation String)
    }


type Msg
    = Nothing


type alias Transformation i =
    { layout : StreamLayout i
    }


init : ( Model, Cmd Msg )
init =
    let
        g =
            Dag.empty identity
                |> node "A" []
                |> node "B" [ "A" ]
                |> node "C" [ "B" ]
                |> node "D" [ "A" ]
                |> node "E" [ "C", "D" ]
                |> node "F" [ "C" ]

        layout1 =
            toFlowLayout g
    in
        ( { dag = g
          , layouts =
                [ { layout = layout1 }
                , { layout = layout1 }
                , { layout = layout1 }
                ]
          }
        , Cmd.none
        )


view : Model -> Html Msg
view model =
    Grid.container
        []
        [ Grid.row
            []
            [ Grid.col
                []
                [ div
                    [ id "accordion"
                    , attribute "role" "tablist"
                    ]
                  <|
                    indexedMap (\i l -> flowGraphCard i (flowGraphWithHeader ( "hallo", l.layout ))) model.layouts
                ]
            ]
        ]


flowGraphCard : Int -> List (Html msg) -> Html msg
flowGraphCard i graphAsHtml =
    div
        [ class "card" ]
        [ div
            [ class "card-header"
            , attribute "role" "tab"
            , id "headingOne"
            ]
            [ h5
                [ class "mb-0" ]
                [ a
                    [ attribute "data-toggle" "collapse"
                    , href <| "#collapse" ++ (toString i)
                    , attribute "aria-expanded" "true"
                    , attribute "aria-controls" <| "collapse" ++ (toString i)
                    ]
                    [ text <| "Collapsible Group Item #" ++ (toString i) ]
                ]
            ]
        , div
            [ id <| "collapse" ++ (toString i)
            , class "collapse show"
            , attribute "role" "tabpanel"
            , attribute "aria-labelledby" "headingOne"
            , attribute "data-parent" "#accordion"
            ]
            [ div
                [ class "card-body" ]
                graphAsHtml
            ]
        ]



--   <div id="collapseOne" class="collapse show" role="tabpanel" aria-labelledby="headingOne" data-parent="#accordion">
--     <div class="card-body">
--       Anim pariatur cliche reprehenderit, enim eiusmod high life accusamus terry richardson ad squid. 3 wolf moon officia aute, non cupidatat skateboard dolor brunch. Food truck quinoa nesciunt laborum eiusmod. Brunch 3 wolf moon tempor, sunt aliqua put a bird on it squid single-origin coffee nulla assumenda shoreditch et. Nihil anim keffiyeh helvetica, craft beer labore wes anderson cred nesciunt sapiente ea proident. Ad vegan excepteur butcher vice lomo. Leggings occaecat craft beer farm-to-table, raw denim aesthetic synth nesciunt you probably haven't heard of them accusamus labore sustainable VHS.
--     </div>
--   </div>
-- </div>


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Nothing ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
