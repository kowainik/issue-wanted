{- | This module contains functions that are used for updating the database.
It combines functionality from @IW.Sync.Search@ and @IW.Db@ to fetch the latest
data and insert it into the database.
-}

module IW.Sync.Update
       ( syncRepos
       ) where

import Control.Monad.IO.Unlift (MonadUnliftIO)
import UnliftIO.Async (mapConcurrently_)

import IW.App (WithError)
import IW.Core.Repo (Repo (..))
import IW.Db (WithDb, upsertRepos, updateRepoCategories)
import IW.Effects.Cabal (MonadCabal (..), getCabalCategories)
import IW.Sync.Search (searchAllHaskellRepos)
import IW.Time (getToday)


-- | This function fetches all repos from the GitHub API, downloads their @.cabal@ files,
-- and upserts them into the database.
syncRepos
    :: forall env m.
       ( MonadCabal m
       , MonadUnliftIO m
       , WithDb env m
       , WithLog env m
       , WithError m
       )
    => Integer -- ^ The starting date interval used in the search function
    -> m ()
syncRepos interval = do
    today <- liftIO getToday
    repos <- searchAllHaskellRepos today interval
    upsertRepos repos
    mapConcurrently_ syncCategories repos

-- | This function takes a @Repo@ and attempts to download its @.cabal@ file.
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
