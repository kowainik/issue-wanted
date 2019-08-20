module Main exposing (..)

import Browser
import Html exposing (Html, text, div, h1, h2, button)
import Html.Attributes exposing (src, class, disabled)
import Html.Events exposing (onClick)

import IW.Api exposing (ResultErr)
import IW.Core.Types exposing (Issue)


---- MODEL ----

type alias Model =
    { issues : List Issue
    , getErr : Bool
    }

init : ( Model, Cmd Msg )
init =
    ( { issues = [], getErr = False }
    , Cmd.none
    )

---- UPDATE ----

type Msg
    = NoOp
    | GetIssues
    | GetIssuesRes (ResultErr (List Issue))
    | Refresh

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model = case msg of
    NoOp -> ( model, Cmd.none )
    GetIssues -> (model, Cmd.none)
    GetIssuesRes res -> case res of
        Ok resIssues -> ({model| issues = resIssues}, Cmd.none)
        Err err -> ({model| getErr = True}, Cmd.none)
    Refresh -> init

---- VIEW ----

view : Model -> Html Msg
view m = div []
    [ h1 [] [text "Issue Wanted testing page"]
    , h2 [] [text "Get 'Issues' endpoint"]
    , button [onClick GetIssues] [text "Get Issues"]
    , div [class "err"] (if m.getErr then [text "Get errored"] else [])
    , button [onClick Refresh] [text "Refresh"]
    ]

---- PROGRAM ----

main : Program () Model Msg
main = Browser.element
    { view = view
    , init = \_ -> init
    , update = update
    , subscriptions = always Sub.none
    }

-- Util
isNothing : Maybe a -> Bool
isNothing x = case x of
    Nothing -> True
    _ -> False
