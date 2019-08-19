module IW.Server
       ( API
       , server
       ) where

import Servant.API ((:<|>))
import Servant.API.Generic (toServant)
import Servant.Server (Server, hoistServer)

import IW.App (AppEnv)
import IW.Effects.Log (runAppAsHandler)
import IW.Server.Issue (IssuesAPI, issuesHandler)
import IW.Server.Repo (ReposAPI, reposHandler)


type API = ReposAPI :<|> IssuesAPI

server :: AppEnv -> Server API
server env = hoistServer (Proxy @API) (runAppAsHandler env) (toServant iwServer)

iwServer :: IssuesSite AppServer
iwServer = IssuesSite
    { reposRoute = reposHandler
    , issuesRoute = issuesHandler
    }
