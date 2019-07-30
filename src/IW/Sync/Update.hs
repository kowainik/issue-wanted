{- | This module contains functions that are used for updating the database.
It combines functionality from @IW.Sync.Search@ and @IW.Db@ to fetch the latest
data and insert it into the database.
-}

module IW.Sync.Update
       ( fetchAndUpsertReposByDate
       ) where

import Control.Monad.IO.Unlift (MonadUnliftIO)
import Data.Time (Day (..))
import UnliftIO.Async (mapConcurrently_)

import IW.App (WithError)
import IW.Core.Repo (Repo (..))
import IW.Db (WithDb, upsertRepos, updateRepoCategories)
import IW.Effects.Cabal (MonadCabal (..), getCabalCategories)
import IW.Sync.Search (fetchHaskellReposByDate, fromGitHubRepo)


-- | This function fetches repos from the GitHub API within a specified date range,
-- parses their @.cabal@ files, and upserts them into the database.
fetchAndUpsertReposByDate
    :: forall env m.
       ( MonadCabal m
       , MonadUnliftIO m
       , WithDb env m
       , WithLog env m
       , WithError m
       )
    => Day
    -> Day
    -> m ()
fetchAndUpsertReposByDate from to = do
    gitHubRepos <- fetchHaskellReposByDate from to 1
    let repos = map fromGitHubRepo gitHubRepos
    upsertRepos repos
    mapConcurrently_ fetchAndUpdateCategories repos
  where
    fetchAndUpdateCategories :: Repo -> m ()
    fetchAndUpdateCategories Repo{..} = do
        categories <- getCabalCategories repoOwner repoName
        updateRepoCategories repoOwner repoName categories
