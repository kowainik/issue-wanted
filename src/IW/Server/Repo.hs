module IW.Server.Repo
       ( -- * API
         RepoAPI
       , reposServer

         -- * Handlers
       , reposHandler
       ) where

import IW.App (WithError)
import IW.Core.Repo (Repo (..), Category (..))
import IW.Core.WithId (WithId (..))
import IW.Db (WithDb, getReposByCategories)
import IW.Server.Types (AppServer, ToApi)


type ReposAPI = ToApi ReposSite

newtype ReposSite route = ReposSite
    { reposRoute :: route
        :- "repos"
        :> ReqBody '[JSON] [Category]
        :> QueryParam "page" Int
        :> Get '[JSON] [WithId Repo]
    } deriving (Generic)

reposServer :: RepoSite AppServer
reposServer = RepoSite
    { reposRoute = reposHandler
    }

reposHandler
    :: ( WithDb env m
       , WithError m
       )
    => [Category]
    -> Maybe Int
    -> m [WithId Issue]
reposHandler categories page = getReposByCategories categories $ fromMaybe 0 page
