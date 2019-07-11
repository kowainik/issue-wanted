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


-- | Describes a monad that can download files from a given @Url@.
class Monad m => MonadDownload m where
    downloadFile :: Url -> m ByteString

instance MonadDownload App where
    downloadFile = downloadFileImpl

type WithDownload env m = (MonadIO m, MonadReader env m, WithError m, WithLog env m, Has Manager env)

downloadFileImpl :: WithDownload env m => Url -> m ByteString
downloadFileImpl Url{..} = do
    man <- grab @Manager
    let req = fromString $ toString unUrl
    log I $ "Attempting to download file from " <> unUrl <> " ..."
    response <- liftIO $ httpLbs req man
    let status = statusCode $ responseStatus response
    let body = responseBody response
    log D $ "Recieved a status code of " <> show status <> " from " <> unUrl
    case status of
        200 -> do
            log I $ "Successfully downloaded file from " <> unUrl
            pure $ toStrict body
        _   -> do
            log W $ "Couldn't download file from " <> unUrl
            throwError notFound
