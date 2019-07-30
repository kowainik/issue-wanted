{-# LANGUAGE MultiWayIf #-}

{- | This module contains functions that are used for updating the database.
It combines functionality from @IW.Sync.Search@ and @IW.Db@ to fetch the latest
data and insert it into the database.
-}

module IW.Sync.Update
       ( fetchAndUpsertReposByDate
       ) where

import Control.Monad.IO.Unlift (MonadUnliftIO)
import Data.Time (Day (..), addDays)
import UnliftIO.Async (mapConcurrently_)

import IW.App (WithError)
import IW.Core.Repo (Repo (..))
import IW.Db (WithDb, upsertRepos, updateRepoCategories)
import IW.Effects.Cabal (MonadCabal (..), getCabalCategories)
import IW.Sync.Search (fetchHaskellReposByDate, fromGitHubRepo)


-- syncRepos
--     :: forall env m.
--        ( MonadCabal m
--        , MonadUnliftIO m
--        , WithDb env m
--        , WithLog env m
--        , WithError m
--        )
--     => m ()
-- syncRepos = do


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
    => Day     -- ^ Most recent day
    -> Integer -- ^ Interval of days before most recent day
    -> Int     -- ^ Page
    -> m ()
fetchAndUpsertReposByDate recent interval page = do
    gitHubRepos <- fetchHaskellReposByDate older recent page
    let resCount = length gitHubRepos
    let repos = map fromGitHubRepo gitHubRepos
    upsertRepos repos
    mapConcurrently_ fetchAndUpdateCategories repos
    if | resCount == 100 -> fetchAndUpsertReposByDate recent interval (page + 1)
       | resCount < 100  -> fetchAndUpsertReposByDate nextRecent interval 1
       | otherwise       -> log E $ "More than 100 results returned on page"
  where
    older :: Day
    older = (negate interval) `addDays` recent

    nextRecent :: Day
    nextRecent = (-1) `addDays` older

    fetchAndUpdateCategories :: Repo -> m ()
    fetchAndUpdateCategories Repo{..} = do
        categories <- getCabalCategories repoOwner repoName
        updateRepoCategories repoOwner repoName categories
