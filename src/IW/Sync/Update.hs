{-# LANGUAGE MultiWayIf #-}

{- | This module contains functions that are used for updating the database.
It combines functionality from @IW.Sync.Search@ and @IW.Db@ to fetch the latest
data and insert it into the database.
-}

module IW.Sync.Update
       ( syncRepos
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
syncRepos
    :: forall env m.
       ( MonadCabal m
       , MonadUnliftIO m
       , WithDb env m
       , WithLog env m
       , WithError m
       )
    => Day     -- ^ Oldest day
    -> Day     -- ^ Most recent day
    -> Integer -- ^ Interval of days before most recent day
    -> Int     -- ^ Page
    -> m ()
syncRepos oldest recent interval page =
    if recent == oldest then
        log I $ "Oldest day " <> julianDayToIso oldest <> " reached..."
    else
        do
            resCount <- syncReposByDate intervalStart recent page
            if resCount < 100 then
                syncRepos oldest nextRecent interval 1
            else
                syncRepos oldest recent interval (page + 1)
  where
    intervalStart :: Day
    intervalStart = (negate interval) `addDays` recent

    nextRecent :: Day
    nextRecent = pred intervalStart

syncReposByDate
    :: forall env m.
       ( MonadCabal m
       , MonadUnliftIO m
       , WithDb env m
       , WithLog env m
       , WithError m
       )
    => Day
    -> Day
    -> Int
    -> m Int
syncReposByDate from to page = do
    gitHubRepos <- fetchHaskellReposByDate from to page
    let repos = map fromGitHubRepo gitHubRepos
    upsertRepos repos
    mapConcurrently_ syncCategories repos
    pure $ length repos

syncCategories
    :: forall env m.
       ( MonadCabal m
       , MonadUnliftIO m
       , WithDb env m
       , WithLog env m
       , WithError m
       )
    => Repo
    -> m ()
syncCategories Repo{..} = do
    categories <- getCabalCategories repoOwner repoName
    updateRepoCategories repoOwner repoName categories
