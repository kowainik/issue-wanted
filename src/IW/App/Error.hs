{-# LANGUAGE DeriveAnyClass  #-}

module IW.App.Error
       ( AppError (..)
       , AppErrorType
       , AppException (..)
       , WithError
       , throwError
       , toHttpError

         -- * Error checks
       , isServerError
       , isNotAllowed
       , isInvalid

         -- * Internal error helpers
       , notFound
       , serverError
       , notAllowed
       , invalid
       , missingHeader
       , headerDecodeError
       , dbError

         -- * Error throwing helpers
       , throwOnNothing
       , throwOnNothingM
       , notFoundOnNothing
       , notFoundOnNothingM
       ) where

import Control.Monad.Except (MonadError)
import Data.CaseInsensitive (foldedCase)
import GHC.Stack (SrcLoc (SrcLoc, srcLocModule, srcLocStartLine))
import Network.HTTP.Types.Header (HeaderName)
import Servant.Server (err401, err404, err417, err500, errBody)

import qualified Control.Monad.Except as E (throwError)
import qualified Servant.Server as Servant (ServerError)


-- | Type alias for errors.
type WithError m = (MonadError AppError m, HasCallStack)

-- | Specialized version of 'E.throwError'
throwError :: WithError m => AppErrorType -> m a
throwError = E.throwError . AppError (toSourcePosition callStack)
{-# INLINE throwError #-}

newtype SourcePosition = SourcePosition Text
    deriving newtype (Show, Eq)

-- | Display 'CallStack' as 'SourcePosition' in a format: @Module.function#line_number@.
toSourcePosition :: CallStack -> SourcePosition
toSourcePosition cs = SourcePosition showCallStack
  where
    showCallStack :: Text
    showCallStack = case getCallStack cs of
        []                             -> "<unknown loc>"
        [(name, loc)]                  -> showLoc name loc
        (_, loc) : (callerName, _) : _ -> showLoc callerName loc

    showLoc :: String -> SrcLoc -> Text
    showLoc name SrcLoc{..} =
        toText srcLocModule <> "." <> toText name <> "#" <> show srcLocStartLine

{- | Exception wrapper around 'AppError'. Useful when you need to throw/catch
'AppError' as 'Exception'.
-}
newtype AppException = AppException
    { unAppException :: AppError
    } deriving (Show)
      deriving anyclass (Exception)

-- | 'AppErrorType' with the corresponding 'CallStack'.
data AppError = AppError
    { appErrorCallStack :: !SourcePosition
    , appErrorType      :: !AppErrorType
    } deriving (Show, Eq)

-- | App errors type.
newtype AppErrorType = InternalError IError
    deriving (Show, Eq)

{- | The internal errors that can be thrown. These errors are meant to be
handled within the application and cover exceptional circumstances/coding errors.
-}
data IError
    {- | General not found. -}
    = NotFound
    {- | Some exceptional circumstance has happened stop execution and return.
    Optional text to provide some context in server logs.
    -}
    | ServerError Text
    {- | A required permission level was not met. Optional text to provide some context. -}
    | NotAllowed Text
    {- | Given inputs do not conform to the expected format or shape. Optional
    text to provide some context in server logs.
    -}
    | Invalid Text
    {- | Some header expected, but not present in header list.
    -}
    | MissingHeader HeaderName
    {- | An authentication header that was required was provided but not in a
    format that the server can understand
    -}
    | HeaderDecodeError Text
    -- | Data base specific errors
    | DbError Text
    deriving (Show, Eq)

-- | Map 'AppError' into a HTTP error code.
toHttpError :: AppError -> Servant.ServerError
toHttpError (AppError _callStack errorType) = case errorType of
    InternalError err -> case err of
        NotFound               -> err404
        ServerError msg        -> err500 { errBody = encodeUtf8 msg }
        NotAllowed msg         -> err401 { errBody = encodeUtf8 msg }
        Invalid msg            -> err417 { errBody = encodeUtf8 msg }
        MissingHeader name     -> err401 { errBody = toLazy $ "Header not found: " <> foldedCase name }
        HeaderDecodeError name -> err401 { errBody = encodeUtf8 $ "Unable to decode header: " <> name }
        DbError e              -> err500 { errBody = encodeUtf8 e }

----------------------------------------------------------------------------
-- Error checks
----------------------------------------------------------------------------

isServerError :: AppErrorType -> Bool
isServerError (InternalError (ServerError _)) = True
isServerError _                               = False

isNotAllowed :: AppErrorType -> Bool
isNotAllowed (InternalError (NotAllowed _)) = True
isNotAllowed _                              = False

isInvalid :: AppErrorType -> Bool
isInvalid (InternalError (Invalid _)) = True
isInvalid _                           = False

----------------------------------------------------------------------------
-- Internal Error helpers
----------------------------------------------------------------------------

notFound :: AppErrorType
notFound = InternalError NotFound

serverError :: Text -> AppErrorType
serverError = InternalError . ServerError

notAllowed :: Text -> AppErrorType
notAllowed = InternalError . NotAllowed

invalid :: Text -> AppErrorType
invalid = InternalError . Invalid

missingHeader :: HeaderName -> AppErrorType
missingHeader = InternalError . MissingHeader

headerDecodeError :: Text -> AppErrorType
headerDecodeError = InternalError . HeaderDecodeError

dbError :: Text -> AppErrorType
dbError = InternalError . DbError

----------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------

-- | Extract the value from a maybe, throwing the given 'AppError' if
-- the value does not exist
throwOnNothing :: WithError m => AppErrorType -> Maybe a -> m a
throwOnNothing err = withFrozenCallStack . maybe (throwError err) pure

-- | Extract the value from a 'Maybe' in @m@, throwing the given 'AppError' if
-- the value does not exist
throwOnNothingM :: WithError m => AppErrorType -> m (Maybe a) -> m a
throwOnNothingM err action = withFrozenCallStack $ action >>= throwOnNothing err

-- | Similar to 'throwOnNothing' but throws a 'NotFound' if the value does not exist
notFoundOnNothing :: WithError m => Maybe a -> m a
notFoundOnNothing = withFrozenCallStack . throwOnNothing notFound

-- | Similar to 'throwOnNothingM' but throws a 'NotFound' if the value does not exist
notFoundOnNothingM :: WithError m => m (Maybe a) -> m a
notFoundOnNothingM = withFrozenCallStack . throwOnNothingM notFound