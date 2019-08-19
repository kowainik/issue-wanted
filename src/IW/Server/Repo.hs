module IW.Server.Repo
       ( -- * API
         ReposApi
       , reposServer

         -- * Handlers
       , reposHandler
       ) where

import IW.App (WithError)
import IW.Core.Repo (Repo (..), Category (..))
import IW.Core.WithId (WithId (..))
import IW.Db (WithDb, getReposByCategories)
import IW.Server.Types (AppServer, ToApi)


type ReposApi = ToApi ReposSite

newtype ReposSite route = ReposSite
    { reposRoute :: route
        :- "repos"
        :> ReqBody '[JSON] [Category]
        :> QueryParam "page" Int
        :> Get '[JSON] [WithId Repo]
    } deriving (Generic)

reposServer :: ReposSite AppServer
reposServer = ReposSite
    { reposRoute = reposHandler
    }

reposHandler
    :: ( WithDb env m
       , WithError m
       )
    => [Category]
    -> Maybe Int
    -> m [WithId Repo]
reposHandler categories page = getReposByCategories categories $ fromMaybe 0 page
