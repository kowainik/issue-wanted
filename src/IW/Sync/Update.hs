{- | This module contains functions that are used for updating the database.
It combines functionality from @IW.Sync.Search@ and @IW.Db@ to fetch the latest
data and insert it into the database.
-}

module IW.Sync.Update
       ( syncCache
       ) where

import Control.Monad.IO.Unlift (MonadUnliftIO)
import UnliftIO.Async (async, mapConcurrently_, wait)

import IW.App (AppErrorType, WithError, catchError)
import IW.Core.Repo (Repo (..))
import IW.Db (WithDb, upsertRepos, updateRepoCategories, upsertIssues)
import IW.Effects.Cabal (MonadCabal (..), getCabalCategories)
import IW.Sync.Search (searchAllHaskellRepos, searchAllHaskellIssues)
import IW.Time (getToday)


-- | Loop for keeping the database up to date with the latest GitHub data.
syncCache
    :: forall env m.
       ( MonadCabal m
       , MonadUnliftIO m
       , WithDb env m
       , WithLog env m
       , WithError m
       )
    => Integer
    -> m ()
syncCache interval = forever $ syncWithGithub interval `catchError` syncErrHandler
  where
    syncErrHandler :: AppErrorType -> m a
    syncErrHandler = undefined

-- | This function synchronizes the database with the latest GitHub data.
syncWithGithub
    :: forall env m.
       ( MonadCabal m
       , MonadUnliftIO m
       , WithDb env m
       , WithLog env m
       , WithError m
       )
    => Integer -- ^ The starting date interval used in the search function
    -> m ()
syncWithGithub interval = do
    log I "Starting synchronization of all repositories and issues..."
    today <- liftIO getToday
    log I "Searching for all Haskell repositories..."
    repos <- searchAllHaskellRepos today interval
    log I "Upserting repositories into the database..."
    upsertRepos repos
    populateCategoriesAsync <- async $ mapConcurrently_ syncCategories repos
    log I "Searching for all Haskell issues..."
    issues <- searchAllHaskellIssues today interval
    log I "Upserting issues into the database..."
    upsertIssues issues
    wait populateCategoriesAsync
    log I "All repositories and issues successfully synchronized"

-- | This function takes a @Repo@ and updates its category field in the database.
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
syncCategories repo = do
    categories <- getCabalCategories repo
    updateRepoCategories repo categories
