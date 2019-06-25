{-# LANGUAGE QuasiQuotes #-}

-- | SQL queries to work with the @issues@ table

module IW.Db.Issue
       ( getIssues
       , getIssueById
       , getIssuesByLabel
       , issueToRow
       , insertIssue
       , insertIssues
       ) where

import IW.App (WithError)
import IW.Core.Issue (Issue (..))
import IW.Core.Repo (Repo)
import IW.Core.Id (Id (..))
import IW.Db.Functions (WithDb, asSingleRow, execute, query, queryRaw)


-- | Get a list of all issues from database
getIssues :: (WithDb env m) => m [Issue]
getIssues = queryRaw [sql|
    SELECT id, number, title, body, url, repo_id
    FROM issues
|]

-- | Get a single issue with the associated id 
getIssueById :: (WithDb env m, WithError m) => Id Issue -> m Issue
getIssueById issueId = asSingleRow $ query [sql|
    SELECT id, number, title, body, url, repo_id
    FROM issues
    WHERE id = ?
|] (Only issueId)

-- | Get a list of issues filtered by label name
getIssuesByLabel :: (WithDb env m) => Text -> m [Issue]
getIssuesByLabel label = query [sql|
    SELECT issues.id, issues.number, issues.title, issues.body, issues.url, issues.repo_id
    FROM issues_labels
    JOIN issues ON issues.id = issues_labels.issue_id
    JOIN labels ON labels.id = issues_labels.label_id 
    WHERE labels.name = ?
|] (Only label)

issueToRow :: Issue -> (Int, Text, Text, Text, Id Repo)
issueToRow Issue{..} = (issueNumber, issueTitle, issueBody, issueUrl, issueRepoId)

-- | Insert a single issue into the database
insertIssue :: (WithDb env m) => Issue -> m ()
insertIssue issue = execute [sql|
    INSERT INTO issues (number, title, body, url, repo_id)
    VALUES (?, ?, ?, ?, ?);
|] . issueToRow $ issue

-- | Insert a list of issues into the database
insertIssues :: (WithDb env m) => [Issue] -> m ()
insertIssues = undefined
