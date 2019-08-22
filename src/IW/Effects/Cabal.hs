{- | This module contains the class definitiion of @MonadCabal@ and
an instance of @MonadCabal@ for the @App@ monad. Instances of
@MonadCabal@ have a @getCabalCategories@ action that returns @[Category]@
given a @RepoOwner@ and @RepoName@. It does so by downloading a @.cabal@ file
and parsing the @category@ field of the file.
-}

module IW.Effects.Cabal
       ( MonadCabal (..)

       -- * Internals
       , getCabalCategoriesImpl
       , repoCabalUrl
       ) where

import Data.Text (splitOn, strip)
import Distribution.PackageDescription
import Distribution.PackageDescription.Parsec (parseGenericPackageDescription,
                                               runParseResult)

import IW.App (App (..), AppErrorType (..), CabalPError (..), WithError,
               throwError)
import IW.Core.Repo (Repo (..), Category (..))
import IW.Effects.Download (MonadDownload (..))


-- | Describes a monad that returns @[Category]@ given a @Repo@.
class Monad m => MonadCabal m where
    getCabalCategories :: Repo -> m [Category]

instance MonadCabal App where
    getCabalCategories = getCabalCategoriesImpl

type WithCabal env m = (MonadDownload m, WithLog env m, WithError m)

{- | This function takes a @Repo@ and either returns @[Category]@, or
throws a @CabalParseError@ error. This function may also throw any one of the
errors inherited by the use of @downloadFile@ defined in @IW.Effects.Download@.
-}
getCabalCategoriesImpl
    :: forall env m.
       WithCabal env m
    => Repo
    -> m [Category]
getCabalCategoriesImpl Repo{..} = do
    cabalFile <- downloadFile repoCabalUrl
    let result = runParseResult $ parseGenericPackageDescription cabalFile
    log D $ "Parsed cabal file downloaded from " <> show repoCabalUrl
            <> " with these warnings: " <> show (fst result)
    case snd result of
        Left err -> do
            let cabalParseErr = CabalParseError $ second (CabalPError <$>) err
            log E $ "Failed to parse cabal file downloaded from " <> show repoCabalUrl
                    <> " with these errors: " <> show cabalParseErr
            throwError cabalParseErr
        Right genPkgDescr -> do
            log I $ "Successfuly parsed cabal file downloaded from " <> show repoCabalUrl
            pure $ categoryNames genPkgDescr

-- | Parses a @GenericPackageDescription@ for @[Category]@.
categoryNames :: GenericPackageDescription -> [Category]
categoryNames genPkgDescr = Category <$> splitCategories genPkgDescr
  where
    splitCategories :: GenericPackageDescription -> [Text]
    splitCategories = splitAndStrip "," . toText . category . packageDescription

    {- | This function takes a delimeter and a delimeter seperated value,
    and returns a list of @Text@ values stripped of excess whitespace.
    Note that it returns an empty list when an empty delimeter seperated value is
    passed in. This prevents the value @[""]@ from being returned.
    -}
    splitAndStrip :: Text -> Text -> [Text]
    splitAndStrip _ ""       = []
    splitAndStrip delim text = strip <$> splitOn delim text
