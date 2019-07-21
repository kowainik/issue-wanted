{-# LANGUAGE QuasiQuotes #-}

-- | SQL queries to work with the @repos@ table.

module IW.Db.Repo
       ( getRepos
       , getReposByCategories
       , upsertRepos
       , updateRepoCategories
       ) where

import IW.App (WithError)
import IW.Core.Repo (Repo, RepoOwner, RepoName, Category)
import IW.Core.SqlArray (SqlArray (..))
import IW.Core.WithId (WithId)
import IW.Db.Functions (WithDb, executeMany, executeNamed, queryNamed, queryRaw)


-- | Returns all repos in the database.
getRepos :: WithDb env m => m [WithId Repo]
getRepos = queryRaw [sql|
    SELECT id, owner, name, descr, categories
    FROM repos
    LIMIT 100
|]

-- | Returns all repos with at least one category in the given list.
getReposByCategories :: (WithDb env m, WithError m) => [Category] -> m [WithId Repo]
getReposByCategories categories = queryNamed [sql|
    SELECT id, owner, name, descr, categories
    FROM repos
    WHERE categories && ?categories
    LIMIT 100
|] [ "categories" =? SqlArray categories ]

-- | Insert a list of repos into the database, but update on conflict.
upsertRepos :: WithDb env m => [Repo] -> m ()
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

-- | Update a repo's categories field.
updateRepoCategories
    :: ( WithDb env m
       , WithError m
       )
    => RepoOwner
    -> RepoName
    -> [Category]
    -> m ()
updateRepoCategories repoOwner repoName categories = void $ executeNamed [sql|
    UPDATE repos
    SET categories = ?categories
    WHERE owner = ?owner
      AND name = ?name
|] [ "categories" =? SqlArray categories
   , "owner" =? repoOwner
   , "name" =? repoName
   ]
