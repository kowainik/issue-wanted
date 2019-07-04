module Main where

import Control.Exception (bracket)
import Hedgehog (Group (..), checkParallel)
import System.IO (hSetEncoding, utf8)
import Test.Hspec (Spec, hspec)
import Test.Hspec.Core.Spec (sequential)

import IW (mkAppEnv)
import IW.App (AppEnv, Env (..))
import IW.Config (loadConfig)
import IW.Db (prepareDb)
import IW.Effects.Log (runAppLogIO_)

import Test.Common (joinSpecs)
import Test.Core.Issue (issueRoundtripProp)
import Test.Core.Repo (repoRoundtripProp)

import qualified Data.Pool as Pool


hspecTests :: AppEnv -> Spec
hspecTests = sequential . joinSpecs "issue-wanted" []

hedgehogTests :: AppEnv -> Group
hedgehogTests env = Group "Roundtrip properties" 
    [ issueRoundtripProp env `named` "fromRow . toRow ≡ id"
    , repoRoundtripProp env `named` "fromRow . toRow ≡ id" 
    ]
  where
    named :: a -> b -> (b, a)
    named = flip (,)

main :: IO ()
main = bracket
    (loadConfig >>= mkAppEnv)
    (\Env{..} -> Pool.destroyAllResources envDbPool)
    runTests
  where
    runTests :: AppEnv -> IO ()
    runTests env = do
        -- fix terminal encoding
        hSetEncoding stdout utf8
        hSetEncoding stderr utf8

        -- setup DB tables
        runAppLogIO_ env prepareDb

        -- run all tests
        hspec $ hspecTests env
        ifM (checkParallel $ hedgehogTests env) exitSuccess exitFailure
