{-# LANGUAGE QuasiQuotes #-}

-- | SQL queries to work with the @users@ table.

module IW.Db.Issue
       ( getIssueById
       , getIssuesByLabel
       ) where

import IW.App (WithError)
import IW.Core.Issue (Issue)
import IW.Db.Functions (WithDb, asSingleRow, query)


getIssueById :: (WithDb env m, WithError m) => Int -> m Issue
getIssueById issueId = asSingleRow $ query [sql|
    SELECT id, issue_number, issue_title, issue_body, issue_url, repo_id
    FROM issues
    WHERE id = ?
|] (Only issueId)

getIssuesByLabel :: (WithDb env m, WithError m) => m [Issue]
getIssuesByLabel = undefined
