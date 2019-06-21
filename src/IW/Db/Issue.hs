{-# LANGUAGE QuasiQuotes #-}

-- | SQL queries to work with the @users@ table.

module IW.Db.Issue
       ( getIssues
       , getIssueById
       , getIssuesByLabel
       ) where

import IW.App (WithError)
import IW.Core.Issue (Issue)
import IW.Db.Functions (WithDb, asSingleRow, query, queryRaw)

getIssues :: (WithDb env m) => m [Issue]
getIssues = queryRaw [sql|
    SELECT id, number, title, body, url, repo_id
    FROM issues
|]

getIssueById :: (WithDb env m, WithError m) => Int -> m Issue
getIssueById issueId = asSingleRow $ query [sql|
    SELECT id, number, title, body, url, repo_id
    FROM issues
    WHERE id = ?
|] (Only issueId)

getIssuesByLabel :: (WithDb env m) => Text -> m [Issue]
getIssuesByLabel label = query [sql|
    SELECT issues.id, issues.number, issues.title, issues.body, issues.url, issues.repo_id
    FROM issues_labels
    JOIN issues ON issues.id = issues_labels.issue_id
    JOIN labels ON labels.id = issues_labels.label_id 
    WHERE labels.name = ?
|] (Only label)
