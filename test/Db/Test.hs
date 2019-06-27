module Db.Test where

import IW (mkAppEnv)
import IW.App.Error (AppError)
import IW.App.Monad (AppEnv)
import IW.Config (loadConfig)
import IW.Core.Issue (Issue (..))
import IW.Core.Id (Id (..))
import IW.Db.Issue (getIssues, getIssueById, getIssuesByLabel)
import IW.Effects.Log (runAppLogIO)


mkGhciEnv :: IO AppEnv 
mkGhciEnv = loadConfig >>= mkAppEnv

testGetIssues :: IO (Either AppError [Issue])
testGetIssues = do 
    env <- mkGhciEnv 
    runAppLogIO env getIssues

testGetIssueById :: Id Issue -> IO (Either AppError Issue)
testGetIssueById issueId = do 
    env <- mkGhciEnv 
    runAppLogIO env $ getIssueById issueId

testGetIssuesByLabel :: Text -> IO (Either AppError [Issue])
testGetIssuesByLabel label = do 
    env <- mkGhciEnv 
    runAppLogIO env $ getIssuesByLabel label
