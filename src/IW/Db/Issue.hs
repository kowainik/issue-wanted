{-# LANGUAGE QuasiQuotes #-}

-- | SQL queries to work with the @users@ table.

module IW.Db.Issue
       ( getIssues
       , getIssueById
       , getIssuesByLabel
       ) where

import IW.App (WithError)
import IW.Core.Issue (Issue (..))
import IW.Core.Id (Id (..))
import IW.Db.Functions (WithDb, asSingleRow, query, queryRaw)


getIssues :: (WithDb env m) => m [Issue]
getIssues = queryRaw [sql|
    SELECT id, number, title, body, url, owner, repo_name, labels
    FROM issues
|]

getIssueById :: (WithDb env m, WithError m) => Id Issue -> m Issue
getIssueById issueId = asSingleRow $ query [sql|
    SELECT id, number, title, body, url, owner, repo_name, labels
    FROM issues
    WHERE id = ?
|] (Only issueId)

getIssuesByLabel :: (WithDb env m) => Text -> m [Issue]
getIssuesByLabel label = query [sql|
    SELECT id, number, title, body, url, owner, repo_name, labels 
    FROM issues 
    WHERE ? = ANY (labels)
|] (Only label)
