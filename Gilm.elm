module Gilm exposing (..)

import Bootstrap.Grid as Grid
import Bootstrap.Form as Form
import Bootstrap.Navbar as Navbar
import Html exposing (Html, ul, li, text, div, form, label, button, input)
import Html.Attributes exposing (value, for, id, type_, class, href, placeholder)
import Html.Events exposing (onClick, onInput, onSubmit)
import Http
import Maybe exposing (..)
import Json.Decode as JD exposing (..)
import Task
import GithubApiToken exposing (apiToken)


main =
    Html.program
        { init = init (Just "<nobody>")
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { navbarState : Navbar.State
    , githubApiToken : String
    , user : Maybe String
    , repos : List String
    }


type Msg
    = Login String
    | UserDataFetched (Result Http.Error String)
    | NavbarMsg Navbar.State
    | Logout


init : Maybe String -> ( Model, Cmd Msg )
init name =
    let
        ( navbarState, navbarCmd ) =
            Navbar.initialState NavbarMsg
    in
        ( { user = name
          , githubApiToken = apiToken
          , repos = []
          , navbarState = navbarState
          }
        , navbarCmd
        )


view : Model -> Html Msg
view model =
    Grid.container []
        [ Grid.row [] [ Grid.col [] [ navbarView model ] ]
        , Grid.row []
            [ Grid.col []
                [ text "Repos"
                ]
            ]
        , Grid.row []
            [ Grid.col []
                [ ul []
                    (List.map (\r -> li [] [ text r ]) model.repos)
                ]
            ]
        ]


navbarView : Model -> Html Msg
navbarView model =
    Navbar.config NavbarMsg
        |> Navbar.withAnimation
        |> Navbar.brand [] [ text "Gilm" ]
        |> Navbar.customItems [Navbar.customItem (navbarUserSectionView model)]
        |> Navbar.view model.navbarState

navbarUserSectionView : Model -> Html Msg
navbarUserSectionView model =
    case model.user of
        Nothing ->
            Html.div
              [ class "form-inline" ]
              [ input
                [ type_ "password"
                , class "form-control my-4 my-sm-0"
                , placeholder "Your Github API Token"
                , Html.Attributes.value model.githubApiToken
                ]
                [  ]
              , input
                [ type_ "button"
                , class "btn ml-2 my-4 my-sm-0"
                , onClick (Login model.githubApiToken)
                , Html.Attributes.value "Log In"
                ]
                []
              ]

        Just username ->
          Form.formInline []
            [ text username
            , input
              [ type_ "button"
              , class "btn ml-2 my-4 my-sm-0"
              , onClick Logout
              , Html.Attributes.value "Log Out"
              ]
              []
            ]

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Login newApiToken->
              ( { model | githubApiToken = newApiToken }, callNewApi newApiToken )

        UserDataFetched (Result.Ok json) ->
            ( { model | repos = [ json ], user = Result.toMaybe ((decodeString (at [ "data", "viewer", "login" ] string) json)) }
            , Cmd.none
            )

        UserDataFetched (Result.Err message) ->
            ( { model | repos = [ toString message ], user = Nothing }, Cmd.none )

        NavbarMsg state ->
            ( { model | navbarState = state }, Cmd.none )

        Logout ->
            ( { model | user = Nothing }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


callNewApi : String -> Cmd Msg
callNewApi apiToken =
    let
        rq =
            Http.request
                { method = "POST"
                , headers = [ Http.header "Authorization" ("bearer " ++ apiToken) ]
                , url = "https://api.github.com/graphql"
                , body = Http.stringBody "application/json" "{ \"query\": \"query { viewer { login }}\""
                , expect = Http.expectString
                , timeout = Nothing
                , withCredentials = False
                }
    in
        Http.send UserDataFetched rq
