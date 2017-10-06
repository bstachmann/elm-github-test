module Github exposing (..)

import Html exposing (Html,ul,li,text,div,form,label,button,input)
import Html.Attributes exposing (value,for,id,type_,class)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as JD exposing (..)
import Task
import GithubApiToken exposing (apiToken)

main =
    Html.program
        { init = init "rpreissel"
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

type alias Model =
    { githubApiToken : String,
      user : String,
      repos : List String
    }

type Msg
  = SampleQueryFetched (Result Http.Error String)
  | TestNewApi String

init : String -> (Model, Cmd Msg)
init name =
  ({user = name, githubApiToken = apiToken, repos = []}, Cmd.none)

view : Model -> Html Msg
view model =
  div [class "container"] [
    div [class "row"] [
      div [class "form-horizontal col-md-4"] [
        div [class "form-group"] [
          label [ for "git-hub-api-token" ] [ text "Github API Token" ],
          input [ class "form-control", id "git-hub-api-token", type_ "password", Html.Attributes.value model.githubApiToken ] [],
          label [ for "username-field" ] [ text "Username" ],
          input [ class "form-control", id "username-field", type_ "text", Html.Attributes.value model.user ] []
        ],
        div [class "form-group"] [
          button [ class "btn btn-primary", onClick (TestNewApi model.githubApiToken) ] [ text "Test new API" ]
        ]
      ]
    ],
    div [class "row"] [
      text "Repos"
    ],
    div [class "row"] [
      ul []
        (List.map (\r -> li [] [ text r ]) model.repos)
    ]
  ]


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    TestNewApi apiToken
      -> (model, callNewApi apiToken)
    SampleQueryFetched (Result.Ok json)
      -> ( { model | repos = [ json ], user = Result.withDefault "<nix>" ((decodeString (at ["data", "viewer", "login"] string) json))}
        , Cmd.none)
    SampleQueryFetched (Result.Err message)
      -> ({model | repos = [ toString message ]}, Cmd.none)


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


callNewApi : String -> Cmd Msg
callNewApi apiToken =
    let
      rq = Http.request
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
