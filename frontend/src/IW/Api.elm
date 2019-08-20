module IW.Api exposing
       ( ResultErr
       )

import Http exposing (Error)
import Json.Decode as D
import Url exposing (Url)


type alias ResultErr a = Result Error a

-- TODO: wire up endpoints
