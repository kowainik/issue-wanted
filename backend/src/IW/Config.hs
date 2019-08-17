{- | Configurations through the @config.toml@ file@.
-}

module IW.Config
       ( Config (..)
       , configT
       , loadConfig
       ) where

import Toml (TomlCodec, (.=))
import qualified Toml


-- | Data type for the configurable elements of the application.
data Config = Config
    { cDbCredentials :: !ByteString
    , cLogSeverity   :: !Severity
    }

-- | TOML codec for the 'Config' data type.
configT :: TomlCodec Config
configT = Config
    <$> Toml.byteString "dbCredentials" .= cDbCredentials
    <*> Toml.read       "log.severity"  .= cLogSeverity

-- | Loads the @config.toml@ file.
loadConfig :: MonadIO m => m Config
loadConfig = Toml.decodeFile configT "config.toml"
