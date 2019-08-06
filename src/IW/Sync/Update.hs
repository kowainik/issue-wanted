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
    => Integer -- ^ Starting interval
    -> m ()
syncRepos interval = do
    repos <- searchAllHaskellRepos interval
    upsertRepos repos
    mapConcurrently_ syncCategories repos

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
