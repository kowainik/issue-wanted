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
import IW.Db.Functions (WithDb, executeMany, executeNamed, queryNamed)


-- | Returns repos in the database by page.
getRepos :: (WithDb env m, WithError m) => Int -> m [WithId Repo]
getRepos page = queryNamed [sql|
    SELECT id, owner, name, descr, categories
    FROM repos
    LIMIT 100
    OFFSET (?page * 100)
|] [ "page" =? page ]

-- | Returns all repos with at least one category in the given list.
getReposByCategories :: (WithDb env m, WithError m) => [Category] -> Int -> m [WithId Repo]
getReposByCategories categories page = queryNamed [sql|
    SELECT id, owner, name, descr, categories
    FROM repos
    WHERE categories && ?categories
    LIMIT 100
    OFFSET (?page * 100)
|] [ "categories" =? SqlArray categories
   , "page" =? page
   ]

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
