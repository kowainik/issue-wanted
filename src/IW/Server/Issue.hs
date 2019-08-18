module IW.Server.Issue
       ( -- * API
         IssueAPI
       , issueServer

         -- * Handlers
       , issuesHandler
       ) where

import IW.App (WithError)
import IW.Core.Issue (Issue (..), Label (..))
import IW.Core.WithId (WithId (..))
import IW.Db (WithDb, getIssuesByLabels)
import IW.Server.Types (AppServer, ToApi)


newtype IssueSite route = IssueSite
    { issuesRoute :: route
        :- "issues"
        :> ReqBody '[JSON] [Label]
        :> QueryParam "page" Int
        :> Get '[JSON] [WithId Issue]
    } deriving (Generic)

type IssueAPI = ToApi IssueSite

issueServer :: IssueSite AppServer
issueServer = IssueSite
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
