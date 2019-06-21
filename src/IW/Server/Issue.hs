module IW.Server.Issue
       ( -- * API
         IssueAPI
       , issueServer

         -- * Handlers
       , issueByIdHandler
       ) where

import IW.App.Error (WithError)
import IW.Core.Issue (Issue (..))
import IW.Db (WithDb, getIssueById)
import IW.Server.Types (AppServer, ToApi)


newtype IssueSite route = IssueSite
    { -- | Fetch issue by issue ID
      issueByIdRoute :: route
        :- "issues"
        :> Capture "id" Int
        :> Get '[JSON] Issue
    } deriving (Generic)

type IssueAPI = ToApi IssueSite

issueServer :: IssueSite AppServer
issueServer = IssueSite
    { issueByIdRoute  = issueByIdHandler
    }

issueByIdHandler
    :: ( WithDb env m
       , WithError m
       )
    => Int
    -> m Issue
issueByIdHandler = getIssueById
