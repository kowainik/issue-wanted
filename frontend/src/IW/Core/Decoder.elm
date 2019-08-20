module IW.Core.Decoder exposing (..)

import Iso8601 as Iso
import Json.Decode as D exposing (..)
import Json.Decode.Pipeline as D exposing (required)

import IW.Core.ElmStreet exposing (..)
import IW.Core.Types exposing (..)


decodeIssue : Decoder Issue
decodeIssue = D.succeed Issue
    |> required "repoOwner" D.string
    |> required "repoName" D.string
    |> required "number" D.int
    |> required "title" D.string
    |> required "body" D.string
    |> required "labels" (D.list D.string)

decodeRepo : Decoder Repo
decodeRepo = D.succeed Repo
    |> required "owner" D.string
    |> required "name" D.string
    |> required "descr" D.string
    |> required "categories" (D.list D.string)
