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

testIssue :: Issue
testIssue = Issue { issueId = Id 57, issueNumber = 45, issueTitle = "Cool issue", issueBody = "Idk??", issueUrl = "hello.com",  issueRepoId = Id 2}

runDbExecute :: (ToRow a) => Sql.Query -> a -> AppEnv -> IO Int64
runDbExecute q arg Env{..} = do
    Pool.withResource envDbPool (\conn -> Sql.execute conn q arg) 
    
testExecute :: (ToRow a) => Sql.Query -> a -> IO Int64
testExecute q arg = loadConfig >>= mkAppEnv >>= runDbExecute q arg

insertIssueTest :: Issue -> IO Int64
insertIssueTest Issue{..} = testExecute [sql|
    INSERT INTO issues (number, title, body, url, repo_id)
    VALUES (?, ?, ?, ?, ?)
|] (issueNumber, issueTitle, issueBody, issueUrl, issueRepoId)
