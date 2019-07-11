{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE TypeSynonymInstances #-}

module IW.App.Env
       ( Env (..)
       , Has (..)
       , grab
       , DbPool
       ) where
    
import Colog (HasLog (..), Message, LogAction)
import Data.Pool (Pool)
import Database.PostgreSQL.Simple (Connection)
import Network.HTTP.Client (Manager)


type DbPool = Pool Connection

data Env (m :: Type -> Type) = Env 
    { envDbPool    :: !DbPool
    , envManager   :: !Manager
    , envLogAction :: !(LogAction m Message)
    } 

instance HasLog (Env m) Message m where
    getLogAction :: Env m -> LogAction m Message
    getLogAction = envLogAction

    setLogAction :: LogAction m Message -> Env m -> Env m
    setLogAction newAction env = env { envLogAction = newAction }

class Has field env where
    obtain :: env -> field

instance Has DbPool                (Env m) where obtain = envDbPool
instance Has Manager               (Env m) where obtain = envManager
instance Has (LogAction m Message) (Env m) where obtain = envLogAction

grab :: forall field env m . (MonadReader env m, Has field env) => m field
grab = asks $ obtain @field
