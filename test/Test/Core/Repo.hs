{-# LANGUAGE QuasiQuotes #-}

module Test.Core.Repo 
       ( repoRoundtripProp
       )where

import IW.App (AppEnv, WithError)
import IW.Core.Id (Id (..))
import IW.Core.Repo (Repo (..))
import IW.Core.SqlArray (SqlArray (..))
import IW.Effects.Log (runAppLogIO)
import IW.Db (WithDb)
import IW.Db.Functions (asSingleRow, query)

import Database.PostgreSQL.Simple.Types ((:.) (..))

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Test.Gen (genRepoOwner, genRepoName)


repoViaSql :: (WithDb env m, WithError m) => Repo -> m Repo
repoViaSql repo = asSingleRow $ query
    [sql| SELECT ?, ?, ?, ?, (? :: TEXT ARRAY) |]
    (Only (repoId repo) :. repo)

repoRoundtripProp :: AppEnv -> Property
repoRoundtripProp env = property $ do
    generatedRepo <- forAll genRepo
    parsedRepo <- liftIO $ runAppLogIO env $ repoViaSql generatedRepo 
    parsedRepo === Right generatedRepo

testCategories :: [Text]
testCategories = 
    [ "FFI"
    , "Text"
    , "Database"
    , "JSON"
    , "Concurrency"
    ]

genRepo :: MonadGen m => m Repo
genRepo = do
    repoId         <- genId
    repoOwner      <- genRepoOwner
    repoName       <- genRepoName
    repoDescr      <- genDescr
    repoCategories <- genCategories

    pure Repo{..} 
  where
    genId :: MonadGen m => m (Id Repo)
    genId = Id <$> Gen.int (Range.constant 1 500)

    genDescr :: MonadGen m => m Text
    genDescr = Gen.text (Range.constant 0 300) Gen.alphaNum

    genCategories :: MonadGen m => m (SqlArray Text)
    genCategories = SqlArray <$> Gen.subsequence testCategories
