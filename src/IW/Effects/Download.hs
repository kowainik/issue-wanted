{- | This module contains the class definitiion of @MonadDownload@ and 
an instance of @MonadDownload@ for the @App@ monad. Instances of 
@MonadDownload@ have a @downloadFile@ action for downloading files from a given URL.
-}

module IW.Effects.Download
       ( MonadDownload (..)
        
       -- * Internals
       , downloadFileImpl
       ) where

import Network.HTTP.Client (Manager, Response (..), httpLbs)
import Network.HTTP.Types (Status (..))

import IW.App (App, Has, WithError, grab, throwError, notFound)   
import IW.Core.Url (Url (..)) 


-- | Describes a monad that can download files from a given URL.
class Monad m => MonadDownload m where
    downloadFile :: Url -> m ByteString

instance MonadDownload App where 
    downloadFile = downloadFileImpl

type WithDownload env m = (MonadIO m, MonadReader env m, WithError m, Has Manager env)

downloadFileImpl :: WithDownload env m => Url -> m ByteString
downloadFileImpl Url{..} = do
    man <- grab @Manager
    let req = fromString $ toString unUrl
    response <- liftIO $ httpLbs req man
    case statusCode $ responseStatus response of
        200 -> pure $ toStrict $ responseBody response
        _   -> throwError notFound