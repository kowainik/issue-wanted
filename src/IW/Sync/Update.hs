{- | This module contains functions that are used for updating the database.
It combines functionality from @IW.Sync.Search@ and @IW.Db@ to fetch the latest
data and insert it into the database.
-}

module IW.Sync.Update
       ( syncRepos
       ) where

import Control.Monad.IO.Unlift (MonadUnliftIO)
import Data.Time (Day (..), addDays)
import Relude.Extra.Enum (next)
import UnliftIO.Async (mapConcurrently_)

import IW.App (WithError)
import IW.Core.Repo (Repo (..))
import IW.Db (WithDb, upsertRepos, updateRepoCategories)
import IW.Effects.Cabal (MonadCabal (..), getCabalCategories)
import IW.Sync.Search (searchHaskellReposByDate, fromGitHubRepo, liftGithubSearchToApp)
import IW.Time (firstHaskellRepoCreated, getToday, julianDayToIso)


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
    => m ()
syncRepos = do
    today <- liftIO getToday
    sync syncReposByDate firstHaskellRepoCreated today 15 1

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
    gitHubRepos <- liftGithubSearchToApp $ searchHaskellReposByDate from to page
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

sync
    :: forall env m.
       ( MonadCabal m
       , MonadUnliftIO m
       , WithDb env m
       , WithLog env m
       , WithError m
       )
    => (Day -> Day -> Int -> m Int) -- ^ The function used for synchronization
    -> Day     -- ^ Oldest day that will be reached
    -> Day     -- ^ Most recent day to start synchronization from
    -> Integer -- ^ Interval of days for the date range to be split into
    -> Int     -- ^ Page
    -> m ()
sync syncFunction oldest recent interval page =
    if recent == oldest
    then log I $ "Oldest day " <> julianDayToIso oldest <> " reached. Synchronization complete."
    else do
        resCount <- syncFunction intervalStart recent page
        if resCount < 100
            then sync syncFunction oldest nextRecent interval 1
            else sync syncFunction oldest recent interval $ next page
  where
    intervalStart :: Day
    intervalStart = negate interval `addDays` recent

    nextRecent :: Day
    nextRecent = pred intervalStart
