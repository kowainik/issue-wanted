module IW.Sync.Update where

import Control.Monad.IO.Unlift (MonadUnliftIO)
import UnliftIO.Async (mapConcurrently_)

import IW.App (WithError)
import IW.Db
import IW.Effects.Cabal (MonadCabal (..))
import IW.Sync.Search


fetchAndUpsertRepos
    ::
    ( MonadCabal m
    , MonadUnliftIO m
    , WithDb env m
    , WithLog env m
    , WithError m
    )
    => m ()
fetchAndUpsertRepos = do
    gitHubRepos <- fetchAllHaskellRepos
    let repos = fromGitHubRepo <$> gitHubRepos
    upsertRepos repos
    mapConcurrently_ updateRepoCategories repos
