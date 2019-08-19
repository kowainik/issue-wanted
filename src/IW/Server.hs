module IW.Server
       ( IwApi
       , server
       ) where

import Servant.API.Generic (toServant)
import Servant.Server (Server, hoistServer)

import IW.App (AppEnv)
import IW.Effects.Log (runAppAsHandler)
import IW.Server.Issue (IssuesApi, issuesHandler)
import IW.Server.Repo (ReposApi, reposHandler)
import IW.Server.Types (AppServer, ToApi)


data IwSite route = IwSite
    { iwIssuesRoute :: route :- IssuesApi
    , iwReposRoute :: route :- ReposApi
    } deriving (Generic)

type IwApi = ToApi IwSite

iwServer :: IwSite AppServer
iwServer = IwSite
    { iwIssuesRoute = issuesHandler
    , iwReposRoute = reposHandler
    }

server :: AppEnv -> Server IwApi
server env = hoistServer (Proxy @IwApi) (runAppAsHandler env) (toServant iwServer)
