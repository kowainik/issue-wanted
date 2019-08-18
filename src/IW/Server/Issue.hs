module IW.Server.Issue
       ( -- * API
         IssuesAPI
       , issuesServer

         -- * Handlers
       , issuesHandler
       ) where

import IW.App (WithError)
import IW.Core.Issue (Issue (..), Label (..))
import IW.Core.WithId (WithId (..))
import IW.Db (WithDb, getIssuesByLabels)
import IW.Server.Types (AppServer, ToApi)


type IssuesAPI = ToApi IssuesSite

newtype IssuesSite route = IssuesSite
    { issuesRoute :: route
        :- "issues"
        :> ReqBody '[JSON] [Label]
        :> QueryParam "page" Int
        :> Get '[JSON] [WithId Issue]
    } deriving (Generic)

issuesServer :: IssuesSite AppServer
issuesServer = IssuesSite
    { issuesRoute = issuesHandler
    }

issuesHandler
    :: ( WithDb env m
       , WithError m
       )
    => [Label]
    -> Maybe Int
    -> m [WithId Issue]
issuesHandler labels page = getIssuesByLabels labels $ fromMaybe 0 page
