module Main exposing (..)

import Browser
import Cmd.Extra exposing (perform)
import Html exposing (Html, text, div, h1, h2, button, input, text)
import Html.Attributes exposing (src, class, disabled, placeholder, type_, value)
import Html.Events exposing (onClick, onInput)

import IW.Api exposing (ResultErr, getIssues)
import IW.Core.Types exposing (Issue)
import IW.Cmd exposing (noCmd)


---- MODEL ----

type alias Model =
    { issues : List Issue
    , page : Int
    , err : Bool
    , labels : List String
    , inputLabel : String
    , categories : List String
    , inputCategory : String
    }

init : ( Model, Cmd Msg )
init = noCmd
    { issues = []
    , page = 0
    , err = False
    , labels = []
    , inputLabel = ""
    , categories = []
    , inputCategory = ""
    }

---- UPDATE ----

type Msg
    = NoOp
    | AddLabel String
    | DeleteLabel String
    | UpdateLabel String
    | AddCategory String
    | DeleteCategory String
    | UpdateCategory String
    | GetIssues
    | GetIssuesRes (ResultErr (List Issue))
    | Refresh

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model = case msg of
    NoOp -> noCmd model
    AddLabel l ->
        ( {model| labels = add l model.labels, inputLabel = ""}
        , perform <| UpdateLabel ""
        )
    DeleteLabel l -> noCmd {model| labels = delete l model.labels}
    UpdateLabel l -> noCmd {model| inputLabel = l}
    AddCategory c ->
        ( {model| categories = add c model.categories, inputCategory = ""}
        , perform <| UpdateCategory ""
        )
    DeleteCategory c -> noCmd {model| categories = delete c model.categories}
    UpdateCategory c -> noCmd {model| inputCategory = c}
    GetIssues -> (model, getIssues model.labels model.page GetIssuesRes)
    GetIssuesRes res -> case res of
        Ok resIssues -> noCmd {model| issues = resIssues}
        Err err -> noCmd {model| err = True}
    Refresh -> init

delete : String -> List String -> List String
delete str l = case l of
    [] -> []
    (cur::rest) -> if str == cur then rest else cur :: delete str rest

add : String -> List String -> List String
add str l =
    if str /= "" && not (List.member str l)
    then str :: l
    else l

---- VIEW ----

view : Model -> Html Msg
view m = div []
    [ h1 [] [text "Issue Wanted testing page"]

    , div []
        [ h2 [] [text "Labels"]
        , div [] <| List.map (\l -> div [class "tag"] [text l, button [onClick <| DeleteLabel l] [text "x"]]) m.labels
        , input [ type_ "text", placeholder "Label...", value m.inputLabel, onInput UpdateLabel] []
        , button [onClick <| AddLabel m.inputLabel] [text "Add"]
        ]

    , div []
        [ h2 [] [text "Categories"]
        , div [] <| List.map (\c -> div [class "tag"] [text c, button [onClick <| DeleteCategory c] [text "x"]]) m.categories
        , input [ type_ "text", placeholder "Category...", value m.inputCategory, onInput UpdateCategory] []
        , button [onClick <| AddCategory m.inputCategory] [text "Add"]
        ]

    , div [] [ button [onClick GetIssues] [text "Get Issues"] ]
    , div [class "err"] (if m.err then [text "Get errored"] else [])
    , div [] [text <| if m.issues == [] then "No Issues" else "Some issues"]
    , div []
        [ button [] [text "<"]
        , text <| String.fromInt m.page
        , button [] [text ">"]
        ]
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
