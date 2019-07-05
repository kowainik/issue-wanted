{-# LANGUAGE QuasiQuotes #-}

-- | SQL queries to work with the @issues@ table.

module IW.Db.Repo
       ( getRepos
       , upsertRepos
       ) where

import IW.Core.Repo (Repo (..))
import IW.Db.Functions (WithDb, executeMany, queryRaw)


-- | Returns all repos in the database
getRepos :: (WithDb env m) => m [Repo]
getRepos = queryRaw [sql|
    SELECT id, owner, name, descr, categories
    FROM repos
|]

-- | Insert a list of repos into the database, but update on conflict
upsertRepos :: (WithDb env m) => [Repo] -> m ()
upsertRepos repos = executeMany [sql|
    INSERT INTO repos 
        (owner, name, descr, categories)
    VALUES 
        (?, ?, ?, ?)
    ON CONFLICT ON CONSTRAINT unique_repos DO 
    UPDATE SET 
        descr = EXCLUDED.descr
      , categories = EXCLUDED.categories;
|] repos
