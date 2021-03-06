{-# LANGUAGE DeriveAnyClass  #-}

module IW.App.Error
       ( AppError (..)
       , AppErrorType (..)
       , AppException (..)
       , CabalPError (..)
       , CabalErrorInfo (..)
       , WithError
       , githubErrToAppErr
       , throwError
       , catchError
       , toHttpError

         -- * Error checks
       , isServerError
       , isNotAllowed
       , isInvalid

         -- * Error throwing helpers
       , throwOnNothing
       , throwOnNothingM
       , notFoundOnNothing
       , notFoundOnNothingM
       ) where

import Control.Monad.Except (MonadError)
import Data.CaseInsensitive (foldedCase)
import Distribution.Parsec.Common (PError (..))
import Distribution.Types.Version (Version)
import GHC.Stack (SrcLoc (SrcLoc, srcLocModule, srcLocStartLine))
import Network.HTTP.Types.Header (HeaderName)
import PgNamed (PgNamedError)
import Servant.Server (err401, err404, err417, err500, errBody)

import IW.Core.Url (Url (..))

import qualified Control.Monad.Except as E (throwError, catchError)
import qualified GitHub
import qualified Servant.Server as Servant (ServerError)


-- | Type alias for errors.
type WithError m = (MonadError AppError m, HasCallStack)

-- | Specialized version of 'E.throwError'.
throwError :: WithError m => AppErrorType -> m a
throwError = E.throwError . AppError (toSourcePosition callStack)
{-# INLINE throwError #-}

-- | Specialized version of 'E.catchError'.
catchError :: WithError m => m a -> (AppErrorType -> m a) -> m a
catchError action handler = action `E.catchError` (handler . appErrorType)
{-# INLINE catchError #-}

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
data AppErrorType
    {- | General not found. -}
    = NotFound
    {- | Some exceptional circumstance has happened to stop execution and return.
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
    format that the server can understand.
    -}
    | HeaderDecodeError Text
    {- | Data base specific errors. -}
    | DbError Text
    {- | Data base named parameters errors. -}
    | DbNamedError PgNamedError
    {- | An error occurred while attempting to parse a @.cabal@ file. -}
    | CabalParseError CabalErrorInfo
    {- | A HTTP error occurred using the @github@ library.
    The actual caught error is included. -}
    | GithubHttpError Text
    {- | An error thrown from one of the @github@ library parsing functions. -}
    | GithubParseError Text
    {- | The JSON returned from a @github@ library function is malformed or unexpected. -}
    | GithubJsonError Text
    {- | An incorrect input was used in a GitHub login attempt. -}
    | GithubUserError Text
    {- | Failed to download the contents of the @Url@. -}
    | UrlDownloadFailed Url
    deriving (Show, Eq)

data CabalErrorInfo = CabalErrorInfo
    { cabalVersion :: Maybe Version
    , cabalPErrors :: [CabalPError]
    } deriving (Show, Eq)

{- | A wrapper around the 'PError' type from the @Cabal@ library.
This is needed to implement an instance of 'Eq' for 'PError' without
triggering the @orphan-instances@ warning. -}
newtype CabalPError = CabalPError
    { unCabalPError :: PError
    } deriving newtype (Show)

instance Eq CabalPError where
    CabalPError (PError pos1 str1) == CabalPError (PError pos2 str2) =
        pos1 == pos2 && str1 == str2

-- | Map the @github@ library's 'GitHub.Error' type into 'AppErrorType'.
githubErrToAppErr :: GitHub.Error -> AppErrorType
githubErrToAppErr = \case
    GitHub.HTTPError e    -> GithubHttpError $ show e
    GitHub.ParseError msg -> GithubParseError msg
    GitHub.JsonError msg  -> GithubJsonError msg
    GitHub.UserError msg  -> GithubUserError msg

-- | Map 'AppError' into a HTTP error code.
toHttpError :: AppError -> Servant.ServerError
toHttpError AppError{..} = case appErrorType of
    NotFound               -> err404
    ServerError msg        -> err500 { errBody = encodeUtf8 msg }
    NotAllowed msg         -> err401 { errBody = encodeUtf8 msg }
    Invalid msg            -> err417 { errBody = encodeUtf8 msg }
    MissingHeader name     -> err401 { errBody = toLazy $ "Header not found: " <> foldedCase name }
    HeaderDecodeError name -> err401 { errBody = encodeUtf8 $ "Unable to decode header: " <> name }
    DbError e              -> err500 { errBody = encodeUtf8 e }
    DbNamedError e         -> err500 { errBody = show e }
    CabalParseError e      -> err500 { errBody = show e }
    GithubHttpError e      -> err500 { errBody = encodeUtf8 e }
    GithubParseError msg   -> err500 { errBody = encodeUtf8 msg }
    GithubJsonError msg    -> err500 { errBody = encodeUtf8 msg }
    GithubUserError msg    -> err500 { errBody = encodeUtf8 msg }
    UrlDownloadFailed url  -> err500 { errBody = encodeUtf8 $ "Couldn't download file from " <> unUrl url }

----------------------------------------------------------------------------
-- Error checks
----------------------------------------------------------------------------

isServerError :: AppErrorType -> Bool
isServerError (ServerError _) = True
isServerError _               = False

isNotAllowed :: AppErrorType -> Bool
isNotAllowed (NotAllowed _) = True
isNotAllowed _              = False

isInvalid :: AppErrorType -> Bool
isInvalid (Invalid _) = True
isInvalid _           = False

----------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------

-- | Extract the value from a 'Maybe', throwing the given 'AppError' if
-- the value does not exist.
throwOnNothing :: WithError m => AppErrorType -> Maybe a -> m a
throwOnNothing err = withFrozenCallStack . maybe (throwError err) pure

-- | Extract the value from a 'Maybe' in @m@, throwing the given 'AppError' if
-- the value does not exist.
throwOnNothingM :: WithError m => AppErrorType -> m (Maybe a) -> m a
throwOnNothingM err action = withFrozenCallStack $ action >>= throwOnNothing err

-- | Similar to 'throwOnNothing' but throws a 'NotFound' error if the value does not exist.
notFoundOnNothing :: WithError m => Maybe a -> m a
notFoundOnNothing = withFrozenCallStack . throwOnNothing NotFound

-- | Similar to 'throwOnNothingM' but throws a 'NotFound' error if the value does not exist.
notFoundOnNothingM :: WithError m => m (Maybe a) -> m a
notFoundOnNothingM = withFrozenCallStack . throwOnNothingM NotFound
