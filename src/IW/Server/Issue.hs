module IW.Server.Issue
       ( -- * API
         IssueAPI
       , issueServer

         -- * Handlers
       , issueByIdHandler
       , issuesHandler
       ) where

import IW.App.Error (WithError)
import IW.Core.Id (Id (..))
import IW.Core.Issue (Issue (..))
import IW.Db (WithDb, getIssues, getIssueById, getIssuesByLabel)
import IW.Server.Types (AppServer, ToApi)


data IssueSite route = IssueSite
    { -- | Fetch issue by issue ID
      issueByIdRoute :: route
        :- "issues"
        :> Capture "id" (Id Issue)
        :> Get '[JSON] Issue
    , issuesRoute :: route
        :- "issues"
        :> QueryParam "label" Text
        :> Get '[JSON] [Issue]
    } deriving (Generic)

type IssueAPI = ToApi IssueSite

issueServer :: IssueSite AppServer
issueServer = IssueSite
    { issueByIdRoute  = issueByIdHandler
    , issuesRoute     = issuesHandler
    }

issueByIdHandler
    :: ( WithDb env m
       , WithError m
       )
    => Id Issue
    -> m Issue
issueByIdHandler = getIssueById

issuesHandler 
    :: ( WithDb env m
       )
    => Maybe Text
    -> m [Issue]
issuesHandler maybeLabel = case maybeLabel of
    Nothing    -> getIssues
    Just label -> getIssuesByLabel label 
