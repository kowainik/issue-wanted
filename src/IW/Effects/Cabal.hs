module IW.Effects.Cabal
       ( getCabalCategories

       -- * Internals
       , getCabalCategoriesImpl
       , repoCabalUrl
       ) where

import Data.Text (splitOn, strip)
import Distribution.PackageDescription
import Distribution.PackageDescription.Parsec (parseGenericPackageDescriptionMaybe)
import Network.HTTP.Client (Manager)

import IW.App (App, Has, WithError)
import IW.Core.Repo (RepoOwner (..), RepoName (..), Category (..))
import IW.Core.Url (Url (..))
import IW.Effects.Download (downloadFileImpl)


class Monad m => MonadCabal m where
    getCabalCategories :: RepoOwner -> RepoName -> m [Category]

instance MonadCabal App where
    getCabalCategories = getCabalCategoriesImpl

type WithCabal env m = (MonadIO m, MonadReader env m, WithError m, Has Manager env)

getCabalCategoriesImpl :: WithCabal env m => RepoOwner -> RepoName -> m [Category]
getCabalCategoriesImpl repoOwner repoName = do
    cabalFile <- downloadFileImpl $ repoCabalUrl repoOwner repoName 
    pure $ case parseGenericPackageDescriptionMaybe cabalFile of
        Nothing -> []
        Just genPkgDescr -> categoryNames genPkgDescr

repoCabalUrl :: RepoOwner -> RepoName -> Url
repoCabalUrl (RepoOwner rO) (RepoName rN) = Url $
    "https://raw.githubusercontent.com/" 
    <> rO
    <> "/" 
    <> rN
    <> "/master/"
    <> rN
    <> ".cabal" 

categoryNames :: GenericPackageDescription -> [Category]
categoryNames genPkgDescr = Category . strip <$> splitCategories genPkgDescr
  where
    splitCategories :: GenericPackageDescription -> [Text]
    splitCategories = splitOn' . toText . category . packageDescription

    splitOn' :: Text -> [Text]
    splitOn' = \case 
        ""   -> []
        text -> splitOn "," text
