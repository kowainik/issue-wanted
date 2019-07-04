module IW.Server.Issue
       ( -- * API
         IssueAPI
       , issueServer

         -- * Handlers
       , issuesHandler
       ) where

import IW.Core.Issue (Issue (..))
import IW.Db (WithDb, getIssues, getIssuesByLabel)
import IW.Server.Types (AppServer, ToApi)


data IssueSite route = IssueSite
    { issuesRoute :: route
        :- "issues"
        :> QueryParam "label" Text
        :> Get '[JSON] [Issue]
    } deriving (Generic)

type IssueAPI = ToApi IssueSite

issueServer :: IssueSite AppServer
issueServer = IssueSite
    { issuesRoute = issuesHandler
    }

issuesHandler 
    :: ( WithDb env m
       )
    => Maybe Text
    -> m [Issue]
issuesHandler maybeLabel = case maybeLabel of
    Nothing    -> getIssues
    Just label -> getIssuesByLabel label 
