{-# LANGUAGE QuasiQuotes #-}

module TestDb where

import IW (mkAppEnv)
import IW.App.Monad (AppEnv)
import IW.App.Env (Env (..))
import IW.Core.Issue (Issue (..))
import IW.Core.Id (Id (..))
import IW.Config (Config (..))
import IW.Db.Issue (issueToRow)
import Toml (TomlCodec, (.=))
import qualified Toml
import qualified Database.PostgreSQL.Simple as Sql
import qualified Data.Pool as Pool


-- | Loads the @config.toml@ file.
loadConfig :: IO Config
loadConfig = Toml.decodeFile configT "../config.toml"

-- | TOML codec for the 'Config' data type.
configT :: TomlCodec Config
configT = Config
    <$> Toml.byteString "dbCredentials" .= cDbCredentials
    <*> Toml.read       "log.severity"  .= cLogSeverity

    
testIssue1 :: Issue
testIssue1 = Issue { issueId     = Id 57
                   , issueNumber = 45
                   , issueTitle  = "Cool issue"
                   , issueBody   = "Great issue for beginner"
                   , issueUrl    = "hello.com"
                   , issueRepoId = Id 2 
                   }

testIssue2 :: Issue
testIssue2 = Issue { issueId     = Id 12
                   , issueNumber = 23
                   , issueTitle  = "Hard issue"
                   , issueBody   = "Don't know how to fix this issue!"
                   , issueUrl    = "bye.com"
                   , issueRepoId = Id 1
                   }

testIssues :: [Issue]
testIssues = [testIssue1, testIssue2]


setupDb :: IO AppEnv 
setupDb = loadConfig >>= mkAppEnv 


runDbExecute :: (ToRow a) => Sql.Query -> a -> AppEnv -> IO Int64
runDbExecute q arg Env{..} = do
    Pool.withResource envDbPool (\conn -> Sql.execute conn q arg) 

runDbExecuteMany :: (ToRow a) => Sql.Query -> [a] -> AppEnv -> IO Int64
runDbExecuteMany q args Env{..} = do
    Pool.withResource envDbPool (\conn -> Sql.executeMany conn q args)

    
testExecute :: (ToRow a) => Sql.Query -> a -> IO Int64
testExecute q arg = setupDb >>= runDbExecute q arg

testExecuteMany :: (ToRow a) => Sql.Query -> [a] -> IO Int64
testExecuteMany q args = setupDb >>= runDbExecuteMany q args


insertIssueTest :: Issue -> IO Int64
insertIssueTest issue = testExecute [sql|
    INSERT INTO issues (number, title, body, url, repo_id)
    VALUES (?, ?, ?, ?, ?)
|] . issueToRow $ issue  

insertIssuesTest :: [Issue] -> IO Int64
insertIssuesTest issues = testExecuteMany [sql|
    INSERT INTO issues (number, title, body, url, repo_id)
    VALUES (?, ?, ?, ?, ?)
|] $ issueToRow <$> issues
