{-# LANGUAGE MultiWayIf #-}

{- | This module contains functions that are used for updating the database.
It combines functionality from @IW.Sync.Search@ and @IW.Db@ to fetch the latest
data and insert it into the database.
-}

module IW.Sync.Update
       ( syncReposByDate
       ) where

import Control.Monad.IO.Unlift (MonadUnliftIO)
import Data.Time (Day (..), addDays)
import UnliftIO.Async (mapConcurrently_)

import IW.App (WithError)
import IW.Core.Repo (Repo (..))
import IW.Db (WithDb, upsertRepos, updateRepoCategories)
import IW.Effects.Cabal (MonadCabal (..), getCabalCategories)
import IW.Sync.Search (fetchHaskellReposByDate, fromGitHubRepo)


-- | This function fetches repos from the GitHub API within a specified date range,
-- parses their @.cabal@ files, and upserts them into the database.
syncReposByDate
    :: forall env m.
       ( MonadCabal m
       , MonadUnliftIO m
       , WithDb env m
       , WithLog env m
       , WithError m
       )
    => Day     -- ^ Most recent day
    -> Integer -- ^ Interval of days before most recent day
    -> Int     -- ^ Page
    -> m ()
syncReposByDate recent interval page = do
    gitHubRepos <- fetchHaskellReposByDate older recent page
    let resCount = length gitHubRepos
    let repos = map fromGitHubRepo gitHubRepos
    upsertRepos repos
    mapConcurrently_ syncCategories repos
    if | recent == (ModifiedJulianDay 58484) -> log I $ "Earliest day reached."
       | resCount == 100 -> syncReposByDate recent interval (page + 1)
       | resCount < 100  -> syncReposByDate nextRecent interval 1
       | otherwise       -> log E $ "More than 100 results returned on page"
  where
    older :: Day
    older = (negate interval) `addDays` recent

    nextRecent :: Day
    nextRecent = (-1) `addDays` older

    syncCategories :: Repo -> m ()
    syncCategories Repo{..} = do
        categories <- getCabalCategories repoOwner repoName
        updateRepoCategories repoOwner repoName categories
