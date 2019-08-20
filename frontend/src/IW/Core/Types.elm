module IW.Core.Types exposing (..)

import Time exposing (Posix)


type alias Issue =
    { repoOwner : String
    , repoName : String
    , number : Int
    , title : String
    , body : String
    , labels : List String
    }

type alias Repo =
    { owner : String
    , name : String
    , descr : String
    , categories : List String
    }
