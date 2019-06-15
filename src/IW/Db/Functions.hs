module IW.Db.Functions where

import IW.App.Env (DbPool, Has, grab)

import qualified Data.Pool as Pool
import qualified Database.PostgreSQL.Simple as Sql


-- | Constraint for monadic actions that wants access to database.
type WithDb env m = (MonadReader env m, Has DbPool env, MonadIO m)

-- | Executes a query without arguments that is not expected to return results.
executeRaw
    :: (WithDb env m)
    => Sql.Query
    -> m ()
executeRaw q = withPool $ \conn -> void $ Sql.execute_ conn q
{-# INLINE executeRaw #-}

-- | Perform action that needs database connection.
withPool :: WithDb env m => (Sql.Connection -> IO b) -> m b
withPool f = do
    pool <- grab @DbPool
    liftIO $ Pool.withResource pool f
{-# INLINE withPool #-}
