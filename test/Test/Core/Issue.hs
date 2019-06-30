{-# LANGUAGE QuasiQuotes #-}

module Test.Core.Issue where

import IW.App (AppEnv)
import IW.Core.Id (Id (..))
import IW.Core.Issue (Issue (..))
import IW.Core.Repo (RepoName (..), RepoOwner (..))
import IW.Core.SqlArray (SqlArray (..))
import IW.Effects.Log (runAppLogIO)
import IW.Db.Functions (asSingleRow, query)

import Database.PostgreSQL.Simple.Types ((:.) (..))

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range


issueRoundtripProp :: AppEnv -> Property
issueRoundtripProp env = property $ do
    generatedIssue <- forAll genIssue
    parsedIssue <- liftIO 
                $ runAppLogIO env  
                $ asSingleRow
                $ query [sql| SELECT ?, ?, ?, ?, ?, ?, ? |] (Only (issueId generatedIssue) :. generatedIssue)
    parsedIssue === Right generatedIssue

testLabels :: [Text]
testLabels = 
    [ "good first issue"
    , "help wanted"
    , "low hanging fruit"
    , "docs"
    , "easy"
    ]

genIssue :: MonadGen m => m Issue
genIssue = do
    issueId <- genId
    issueNumber <- genNumber
    issueTitle <- genTitle
    issueBody <- genBody
    issueRepoOwner <- genRepoOwner
    issueRepoName <- genRepoName
    issueLabels <- genLabels

    pure Issue{..} 
  where
    genId :: MonadGen m => m (Id Issue)
    genId = Id <$> Gen.int (Range.constant 1 500)

    genNumber :: MonadGen m => m Int
    genNumber = Gen.int (Range.constant 1 500)

    genTitle :: MonadGen m => m Text
    genTitle = Gen.text (Range.constant 1 30) Gen.alpha

    genBody :: MonadGen m => m Text 
    genBody = Gen.text (Range.constant 0 50) Gen.alphaNum

    genRepoOwner :: MonadGen m => m RepoOwner
    genRepoOwner = RepoOwner <$> Gen.text (Range.constant 1 20) Gen.alphaNum
    
    genRepoName :: MonadGen m => m RepoName
    genRepoName = RepoName <$> Gen.text (Range.constant 1 20) Gen.alphaNum

    genLabels :: MonadGen m => m (SqlArray Text)
    genLabels = SqlArray <$> Gen.subsequence testLabels
