{-# LANGUAGE QuasiQuotes #-}

-- | SQL queries to work with the @issues@ table.

module IW.Db.Issue
       ( getIssues
       , getIssuesByLabels
       , upsertIssues
       ) where

import IW.App (WithError)
import IW.Core.Issue (Issue (..), Label (..))
import IW.Core.SqlArray (SqlArray (..))
import IW.Core.WithId (WithId)
import IW.Db.Functions (WithDb, executeMany, queryNamed)


-- | Returns all issues in the database.
getIssues :: (WithDb env m, WithError m) => Int -> m [WithId Issue]
getIssues page = queryNamed [sql|
    SELECT id, repo_owner, repo_name, number, title, body, labels
    FROM issues
    LIMIT 100
    OFFSET (?page * 100)
|] [ "page" =? page ]

-- | Returns all issues with at least one label in the given list.
getIssuesByLabels :: (WithDb env m, WithError m) => [Label] -> Int -> m [WithId Issue]
getIssuesByLabels labels page = queryNamed [sql|
    SELECT id, repo_owner, repo_name, number, title, body, labels
    FROM issues
    WHERE labels && ?labels
    LIMIT 100
    OFFSET (?page * 100)
|] [ "labels" =? SqlArray labels
   , "page" =? page
   ]

-- | Insert a list of issues into the database, but update on conflict.
upsertIssues :: (WithDb env m) => [Issue] -> m ()
upsertIssues = executeMany [sql|
    INSERT INTO issues
        (repo_owner, repo_name, number, title, body, labels)
    SELECT
        repo_owner, repo_name, number, title, body, labels
    FROM (VALUES (?, ?, ?, ?, ?, ?)) AS
        new (repo_owner, repo_name, number, title, body, labels)
    WHERE EXISTS (
        SELECT (owner, name)
        FROM repos
        WHERE (repos.owner, repos.name) = (new.repo_owner, new.repo_name)
    )
    ON CONFLICT ON CONSTRAINT unique_issues DO
    UPDATE SET
        title  = EXCLUDED.title
      , body   = EXCLUDED.body
      , labels = EXCLUDED.labels;
|]
