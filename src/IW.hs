module IW
       ( mkAppEnv
       , runServer
       , main
       ) where

import Network.Wai.Handler.Warp (run)
import Servant.Server (serve)

import IW.App (AppEnv, Env (..))
import IW.Config (Config (..), loadConfig)
import IW.Db (initialisePool)
import IW.Effects.Log (mainLogAction)
import IW.Server (API, server)


mkAppEnv :: Config -> IO AppEnv
mkAppEnv Config{..} = do
    -- IO configuration
    envDbPool <- initialisePool cDbCredentials

    -- pure configuration
    let envLogAction = mainLogAction cLogSeverity
    pure Env{..}

runServer :: AppEnv -> IO ()
runServer env@Env{..} = run 8081 application
  where
    application = serve (Proxy @API) (server env)

main :: IO ()
main = loadConfig >>= mkAppEnv >>= runServer
