module Gilm exposing (..)

import Bootstrap.Grid as Grid
import Bootstrap.Navbar as Navbar
import Html exposing (Html, ul, li, text, div, form, label, button, input)
import Html.Attributes exposing (value, for, id, type_, class, href)
import Html.Events exposing (onClick, onInput)
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
    = SampleQueryFetched (Result Http.Error String)
    | TestNewApi String
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
                [ div [ class "form-group" ]
                    [ label [ for "username-field" ] [ text "Username" ]
                    , input [ class "form-control", id "username-field", type_ "text", Html.Attributes.value (Maybe.withDefault "xx" (model.user)) ] []
                    ]
                ]
            ]
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
        |> Navbar.customItems
            (case model.user of
                Nothing ->
                    [ Navbar.customItem ( input [ type_ "password", Html.Attributes.value model.githubApiToken ] [ text "init" ] )
                    , Navbar.customItem ( button [ onClick (TestNewApi model.githubApiToken) ] [ text "Login" ] )
                    ]
                Just username ->
                    [ Navbar.customItem ( label [] [ text username ] )
                    , Navbar.customItem ( button [ onClick (Logout) ] [ text "Log out" ] )
                    ]
             )
        |> Navbar.view model.navbarState


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TestNewApi apiToken ->
            ( model, callNewApi apiToken )

        SampleQueryFetched (Result.Ok json) ->
            ( { model | repos = [ json ], user = Result.toMaybe ((decodeString (at [ "data", "viewer", "login" ] string) json)) }
            , Cmd.none
            )

        SampleQueryFetched (Result.Err message) ->
            ( { model | repos = [ toString message ], user = Nothing }, Cmd.none )

        NavbarMsg state ->
            ( { model | navbarState = state }, Cmd.none )

        Logout ->
            ( { model | user = Nothing  }, Cmd.none )


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
        Http.send SampleQueryFetched rq
