module IW.Server.Issue
       ( -- * API
         IssueAPI
       , issueServer

         -- * Handlers
       , issuesHandler
       ) where

import IW.Core.Issue (Issue (..), Label (..))
import IW.Core.WithId (WithId (..))
import IW.Db (WithDb, getIssuesByLabels)
import IW.Server.Types (AppServer, ToApi)


newtype IssueSite route = IssueSite
    { issuesRoute :: route
        :- "issues"
        :> ReqBody '[JSON] [Label]
        :> Get '[JSON] [WithId Issue]
    } deriving (Generic)

type IssueAPI = ToApi IssueSite

issueServer :: IssueSite AppServer
issueServer = IssueSite
    { issuesRoute = issuesHandler
    }

issuesHandler
    :: ( WithDb env m
       )
    => [Label]
    -> m [WithId Issue]
issuesHandler = getIssuesByLabels
