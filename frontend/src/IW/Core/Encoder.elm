module IW.Core.Encoder exposing (..)

import Iso8601 as Iso
import Json.Encode as E exposing (..)

import IW.Core.ElmStreet exposing (..)
import IW.Core.Types exposing (..)


encodeIssue : Issue -> Value
encodeIssue x = E.object
    [ ("repoOwner", E.string x.repoOwner)
    , ("repoName", E.string x.repoName)
    , ("number", E.int x.number)
    , ("title", E.string x.title)
    , ("body", E.string x.body)
    , ("labels", E.list E.string x.labels)
    ]

encodeRepo : Repo -> Value
encodeRepo x = E.object
    [ ("owner", E.string x.owner)
    , ("name", E.string x.name)
    , ("descr", E.string x.descr)
    , ("categories", E.list E.string x.categories)
    ]
