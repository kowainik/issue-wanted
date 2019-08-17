module IW.Server
       ( API
       , server
       ) where

import Servant.API.Generic (toServant)
import Servant.Server (Server, hoistServer)

import IW.App (AppEnv)
import IW.Effects.Log (runAppAsHandler)
import IW.Server.Issue (IssueAPI, issueServer)


type API = IssueAPI

server :: AppEnv -> Server API
server env = hoistServer (Proxy @API) (runAppAsHandler env) (toServant issueServer)
