{-# LANGUAGE QuasiQuotes #-}

-- | SQL queries to work with the @issues@ table.

module IW.Db.Issue
       ( getIssues
       , getIssueById
       , getIssuesByLabel
       ) where

import IW.App (WithError)
import IW.Core.Issue (Issue (..))
import IW.Core.Id (Id (..))
import IW.Db.Functions (WithDb, asSingleRow, query, queryRaw)

-- | Returns all issues in the database
getIssues :: (WithDb env m) => m [Issue]
getIssues = queryRaw [sql|
    SELECT id, number, title, body, repo_owner, repo_name, labels
    FROM issues
|]

-- | Returns a single issue with the corresponding ID
getIssueById :: (WithDb env m, WithError m) => Id Issue -> m Issue
getIssueById issueId = asSingleRow $ query [sql|
    SELECT id, number, title, body, repo_owner, repo_name, labels
    FROM issues
    WHERE id = ?
|] (Only issueId)

-- | Returns a list of issues filtered by label name
getIssuesByLabel :: (WithDb env m) => Text -> m [Issue]
getIssuesByLabel label = query [sql|
    SELECT id, number, title, body, repo_owner, repo_name, labels 
    FROM issues 
    WHERE ? = ANY (labels)
|] (Only label)
