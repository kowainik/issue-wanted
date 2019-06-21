module IW.Server.Issue
       ( -- * API
         IssueAPI
       , issueServer

         -- * Handlers
       , issueByIdHandler
       , issuesHandler
       ) where

import IW.App.Error (WithError)
import IW.Core.Issue (Issue (..))
import IW.Db (WithDb, getIssues, getIssueById, getIssuesByLabel)
import IW.Server.Types (AppServer, ToApi)

data Sort = Id | Title
  deriving (Generic, Show, Eq)

instance FromHttpApiData Sort where
    parseQueryParam :: Text -> Either Text Sort
    parseQueryParam "id"    = Right Id
    parseQueryParam "title" = Right Title
    parseQueryParam _       = Left "Error parsing sort value"

data IssueSite route = IssueSite
    { -- | Fetch issue by issue ID
      issueByIdRoute :: route
        :- "issues"
        :> Capture "id" Int
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
    => Int
    -> m Issue
issueByIdHandler = getIssueById

issuesHandler 
    :: ( WithDb env m
       , WithError m
       )
    => Maybe Text
    -> m [Issue]
issuesHandler maybeLabel = case maybeLabel of
    Nothing    -> getIssues
    Just label -> getIssuesByLabel label 
