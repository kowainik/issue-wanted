{-# LANGUAGE QuasiQuotes #-}

module Test.Core.Repo 
       ( repoRoundtripProp
       ) where

import Hedgehog (MonadGen, Property, forAll, property, (===))

import IW.App (AppEnv, WithError)
import IW.Core.Repo (Repo (..), Category (..))
import IW.Core.SqlArray (SqlArray (..))
import IW.Core.WithId (WithId (..))
import IW.Effects.Log (runAppLogIO)
import IW.Db (WithDb)
import IW.Db.Functions (asSingleRow, query)
import Test.Gen (genId, genRepoOwner, genRepoName)

import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range


repoViaSql :: (WithDb env m, WithError m) => WithId Repo -> m (WithId Repo)
repoViaSql = asSingleRow . query [sql| SELECT ?, ?, ?, ?, (? :: TEXT ARRAY) |]

repoRoundtripProp :: AppEnv -> Property
repoRoundtripProp env = property $ do
    generatedRepo <- forAll genRepo
    parsedRepo <- liftIO $ runAppLogIO env $ repoViaSql generatedRepo 
    parsedRepo === Right generatedRepo

testCategories :: [Category]
testCategories =
    Category
    <$> 
    [ "FFI"
    , "Text"
    , "Database"
    , "JSON"
    , "Concurrency"
    ]

genRepo :: MonadGen m => m (WithId Repo)
genRepo = do
    repoId         <- genId
    repoOwner      <- genRepoOwner
    repoName       <- genRepoName
    repoDescr      <- genDescr
    repoCategories <- genCategories

    pure $ WithId repoId Repo{..} 
  where
    genDescr :: MonadGen m => m Text
    genDescr = Gen.text (Range.constant 0 300) Gen.alphaNum

    genCategories :: MonadGen m => m (SqlArray Category)
    genCategories = SqlArray <$> Gen.subsequence testCategories
