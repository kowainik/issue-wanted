{-# LANGUAGE QuasiQuotes #-}

-- | SQL queries to work with the @repos@ table.

module IW.Db.Repo
       ( getRepos
       , getReposByCategories
       , upsertRepos
       ) where

import IW.Core.Repo (Repo (..), Category (..))
import IW.Core.SqlArray (SqlArray (..))
import IW.Core.WithId (WithId)
import IW.Db.Functions (WithDb, executeMany, query, queryRaw)


-- | Returns all repos in the database.
getRepos :: (WithDb env m) => m [WithId Repo]
getRepos = queryRaw [sql|
    SELECT id, owner, name, descr, categories
    FROM repos
    LIMIT 100
|]

-- | Returns all repos with at least one category in the given list.
getReposByCategories :: (WithDb env m) => [Category] -> m [WithId Repo]
getReposByCategories = query [sql|
    SELECT id, owner, name, descr, categories
    FROM repos
    WHERE ? && categories
    LIMIT 100
|] . Only . SqlArray

-- | Insert a list of repos into the database, but update on conflict.
upsertRepos :: (WithDb env m) => [Repo] -> m ()
upsertRepos = executeMany [sql|
    INSERT INTO repos
        (owner, name, descr, categories)
    VALUES
        (?, ?, ?, ?)
    ON CONFLICT ON CONSTRAINT unique_repos DO
    UPDATE SET
        descr = EXCLUDED.descr
      , categories = EXCLUDED.categories;
|]
