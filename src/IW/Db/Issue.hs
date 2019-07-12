{-# LANGUAGE QuasiQuotes #-}

-- | SQL queries to work with the @issues@ table.

module IW.Db.Issue
       ( getIssues
       , getIssuesByLabel
       , upsertIssues
       ) where

import IW.Core.Issue (Issue (..))
import IW.Core.WithId (WithId)
import IW.Db.Functions (WithDb, executeMany, query, queryRaw)


-- | Returns all issues in the database
getIssues :: (WithDb env m) => m [WithId Issue]
getIssues = queryRaw [sql|
    SELECT id, repo_owner, repo_name, number, title, body, labels
    FROM issues
    LIMIT 100
|]

-- | Returns a list of issues filtered by label name
getIssuesByLabel :: (WithDb env m) => Text -> m [WithId Issue]
getIssuesByLabel = query [sql|
    SELECT id, repo_owner, repo_name, number, title, body, labels
    FROM issues
    WHERE ? = ANY (labels)
    LIMIT 100
|] . Only

-- | Insert a list of issues into the database, but update on conflict
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
