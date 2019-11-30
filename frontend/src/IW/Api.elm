module IW.Api exposing
       ( ResultErr
       , getIssues
       )

import Http exposing (Error)
import Json.Decode as D
import Json.Encode as E
import Url exposing (Url)

import IW.Core.Types exposing (Issue)
import IW.Core.Decoder exposing (decodeIssue)


type alias ResultErr a = Result Error a

getIssues : List String -> Int -> (ResultErr (List Issue) -> msg) -> Cmd msg
getIssues labels page f = Http.request
    { method  = "GET"
    , headers = []
    , url     = "/issues?page=" ++ String.fromInt page
    , body    = Http.jsonBody (E.list E.string labels)
    , expect  = Http.expectJson f (D.list decodeIssue)
    , timeout = Nothing
    , tracker = Nothing
    }
