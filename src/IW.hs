module IW
       ( mkAppEnv
       , mkGhciEnv
       , runServer
       , main
       ) where

import Network.HTTP.Client.TLS (newTlsManager)
import Network.Wai.Handler.Warp (run)
import Servant.Server (serve)

import IW.App (AppEnv, Env (..))
import IW.Config (Config (..), loadConfig)
import IW.Db (initialisePool)
import IW.Effects.Log (mainLogAction)
import IW.Server (IwApi, server)


mkAppEnv :: Config -> IO AppEnv
mkAppEnv Config{..} = do
    -- IO configuration
    envDbPool <- initialisePool cDbCredentials

    -- Http manager configuration
    envManager <- newTlsManager

    -- pure configuration
    let envLogAction = mainLogAction cLogSeverity
    pure Env{..}

mkGhciEnv :: IO AppEnv
mkGhciEnv = loadConfig >>= mkAppEnv

runServer :: AppEnv -> IO ()
runServer env@Env{..} = run 8080 application
  where
    application = serve (Proxy @IwApi) (server env)

main :: IO ()
main = loadConfig >>= mkAppEnv >>= runServer
