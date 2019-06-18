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

getIssues :: (WithDb env m, WithError m) => m [Issue]
getIssues = queryRaw [sql|
    SELECT *
    FROM issues
|]

getIssueById :: (WithDb env m, WithError m) => Int -> m Issue
getIssueById issueId = asSingleRow $ query [sql|
    SELECT *
    FROM issues
    WHERE id = ?
|] (Only issueId)

getIssuesByLabel :: (WithDb env m, WithError m) => Text -> m [Issue]
getIssuesByLabel label = query [sql|
    SELECT issues.*
    FROM issues_labels
    JOIN issues ON issues.id = issues_labels.issue_id
    JOIN labels ON labels.id = issues_labels.label_id 
    WHERE labels.label_name = ?
|] (Only label)
