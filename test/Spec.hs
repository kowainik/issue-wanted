module Main where

import Control.Exception (bracket)
import System.IO (hSetEncoding, utf8)

import IW (mkAppEnv)
import IW.App (AppEnv, Env (..))
import IW.Config (loadConfig)
import IW.Db (prepareDb)
import IW.Effects.Log (runAppLogIO_)

import qualified Data.Pool as Pool


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
