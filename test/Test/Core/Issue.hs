{-# LANGUAGE QuasiQuotes #-}

module Test.Core.Issue 
       ( issueRoundtripProp
       ) where

import IW.App (AppEnv, WithError)
import IW.Core.Id (Id (..))
import IW.Core.Issue (Issue (..))
import IW.Core.SqlArray (SqlArray (..))
import IW.Effects.Log (runAppLogIO)
import IW.Db (WithDb)
import IW.Db.Functions (asSingleRow, query)

import Database.PostgreSQL.Simple.Types ((:.) (..))

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Test.Gen (genRepoOwner, genRepoName)


issueViaSql :: (WithDb env m, WithError m) => Issue -> m Issue
issueViaSql issue = asSingleRow $ query
    [sql| SELECT ?, ?, ?, ?, ?, ?, (? :: TEXT ARRAY) |]
    (Only (issueId issue) :. issue)

issueRoundtripProp :: AppEnv -> Property
issueRoundtripProp env = property $ do
    generatedIssue <- forAll genIssue
    parsedIssue <- liftIO $ runAppLogIO env $ issueViaSql generatedIssue 
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
    issueId        <- genId
    issueRepoOwner <- genRepoOwner
    issueRepoName  <- genRepoName
    issueNumber    <- genNumber
    issueTitle     <- genTitle
    issueBody      <- genBody
    issueLabels    <- genLabels

    pure Issue{..} 
  where
    genId :: MonadGen m => m (Id Issue)
    genId = Id <$> Gen.int (Range.constant 1 500)

    genNumber :: MonadGen m => m Int
    genNumber = Gen.int (Range.constant 1 500)

    genTitle :: MonadGen m => m Text
    genTitle = Gen.text (Range.constant 1 30) Gen.alphaNum

    genBody :: MonadGen m => m Text 
    genBody = Gen.text (Range.constant 0 50) Gen.alphaNum

    genLabels :: MonadGen m => m (SqlArray Text)
    genLabels = SqlArray <$> Gen.subsequence testLabels
