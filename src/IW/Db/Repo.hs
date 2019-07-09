{-# LANGUAGE QuasiQuotes #-}

-- | SQL queries to work with the @repos@ table.

module IW.Db.Repo
       ( getRepos
       , getReposByCategory
       , upsertRepos
       ) where

import IW.Core.Repo (Repo (..))
import IW.Core.WithId (WithId)
import IW.Db.Functions (WithDb, executeMany, query, queryRaw)


-- | Returns all repos in the database
getRepos :: (WithDb env m) => m [WithId Repo]
getRepos = queryRaw [sql|
    SELECT id, owner, name, descr, categories
    FROM repos
    LIMIT 100
|]

-- | Returns a list of repos filtered by category name
getReposByCategory :: (WithDb env m) => Text -> m [WithId Repo]
getReposByCategory = query [sql|
    SELECT id, owner, name, descr, categories
    FROM repos
    WHERE ? = ANY (categories)
    LIMIT 100
|] . Only

-- | Insert a list of repos into the database, but update on conflict
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
