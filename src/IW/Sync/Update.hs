module IW.Sync.Update
       (fetchAndUpsertRepos
       ) where

import Control.Monad.IO.Unlift (MonadUnliftIO)
import UnliftIO.Async (mapConcurrently_)

import IW.App (WithError)
import IW.Core.Repo (Repo (..))
import IW.Db (WithDb, upsertRepos, updateRepoCategories)
import IW.Effects.Cabal (MonadCabal (..), getCabalCategories)
import IW.Sync.Search (fetchAllHaskellRepos, fromGitHubRepo)


fetchAndUpsertRepos
    :: forall env m.
       ( MonadCabal m
       , MonadUnliftIO m
       , WithDb env m
       , WithLog env m
       , WithError m
       )
    => m ()
fetchAndUpsertRepos = do
    gitHubRepos <- fetchAllHaskellRepos
    let repos = map fromGitHubRepo gitHubRepos
    upsertRepos repos
    mapConcurrently_ fetchAndUpdateCategories repos
  where
    fetchAndUpdateCategories :: Repo -> m ()
    fetchAndUpdateCategories Repo{..} = do
        categories <- getCabalCategories repoOwner repoName
        updateRepoCategories repoOwner repoName categories
