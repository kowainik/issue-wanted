module Test.Gen where

import Hedgehog (MonadGen)

import IW.Core.Id (Id (..))
import IW.Core.Repo (RepoName (..), RepoOwner (..))

import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range


genId :: MonadGen m => m (Id a)
genId = Id <$> Gen.int (Range.constant 1 500)

genRepoOwner :: MonadGen m => m RepoOwner
genRepoOwner = RepoOwner <$> Gen.text (Range.constant 1 20) Gen.alphaNum

genRepoName :: MonadGen m => m RepoName
genRepoName = RepoName <$> Gen.text (Range.constant 1 20) Gen.alphaNum
