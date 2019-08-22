{- | This module contains the class definitiion of @MonadDownload@ and
an instance of @MonadDownload@ for the @App@ monad. Instances of
@MonadDownload@ have a @downloadFile@ action for downloading files from a given URL.
-}

module IW.Effects.Download
       ( MonadDownload (..)
       , downloadFileMaybe

       -- * Internals
       , downloadFileImpl
       ) where

import Network.HTTP.Client (Manager, Response (..), httpLbs)
import Network.HTTP.Types (Status (..))

import IW.App (App, AppErrorType (..), Has, WithError, grab, throwError, catchError)
import IW.Core.Url (Url (..))


-- | Describes a monad that can download files from a given @Url@.
class Monad m => MonadDownload m where
    downloadFile :: Url -> m ByteString

instance MonadDownload App where
    downloadFile = downloadFileImpl

type WithDownload env m = (MonadIO m, MonadReader env m, WithError m, WithLog env m, Has Manager env)

{- | This function takes an @Url@ and either returns a @ByteString@ representing
the file contents, or throws an @UrlDownloadFailed@ error.
-}
downloadFileImpl :: WithDownload env m => Url -> m ByteString
downloadFileImpl url@Url{..} = do
    man <- grab @Manager
    let req = fromString $ toString unUrl
    log D $ "Attempting to download file from " <> unUrl <> " ..."
    response <- liftIO $ httpLbs req man
    let status = statusCode $ responseStatus response
    let body = responseBody response
    log D $ "Recieved a status code of " <> show status <> " from " <> unUrl
    case status of
        200 -> do
            log I $ "Successfully downloaded file from " <> unUrl
            pure $ toStrict body
        _   -> do
            log E $ "Couldn't download file from " <> unUrl
            throwError $ UrlDownloadFailed url

-- | A verison of @downloadFile@ that returns a @Maybe BytseString@,
downloadFileMaybe :: (MonadDownload m, WithError m) => Url -> m (Maybe ByteString)
downloadFileMaybe url = (Just <$> downloadFile url) `catchError` \case
    UrlDownloadFailed _ -> pure Nothing
    err -> throwError err
