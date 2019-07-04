module Test.Gen where

import IW.Core.Repo (RepoName (..), RepoOwner (..))

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range


genRepoOwner :: MonadGen m => m RepoOwner
genRepoOwner = RepoOwner <$> Gen.text (Range.constant 1 20) Gen.alphaNum

genRepoName :: MonadGen m => m RepoName
genRepoName = RepoName <$> Gen.text (Range.constant 1 20) Gen.alphaNum
