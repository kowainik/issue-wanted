module IW.Sync.Cabal
       ( fetchRepoCategories
       ) where

import Data.Text (splitOn, strip)
import Distribution.PackageDescription
import Distribution.PackageDescription.Parsec (parseGenericPackageDescriptionMaybe)

import IW.Core.Repo (Repo (..), RepoOwner (..), RepoName (..), Category (..))
import IW.Core.SqlArray (SqlArray (..))
import IW.Core.Url (Url (..))
import IW.Effects.Download (MonadDownload (..))


repoCabalFileUrl :: Repo -> Url
repoCabalFileUrl Repo{..} = Url $
    "https://raw.githubusercontent.com/" 
    <> unRepoOwner repoOwner
    <> "/" 
    <> unRepoName repoName
    <> "/master/"
    <> unRepoName repoName
    <> ".cabal" 

fetchRepoCategories :: MonadDownload m => Repo -> m (SqlArray Category)
fetchRepoCategories repo = do
    cabalFile <- downloadFile $ repoCabalFileUrl repo 
    pure $ case parseGenericPackageDescriptionMaybe cabalFile of
        Nothing -> SqlArray []
        Just genPkgDescr -> SqlArray $ categoryNames genPkgDescr

categoryNames :: GenericPackageDescription -> [Category]
categoryNames genPkgDescr = Category . strip <$> splitCategories genPkgDescr
  where
    splitCategories :: GenericPackageDescription -> [Text]
    splitCategories = splitOn' . toText . category . packageDescription

    splitOn' :: Text -> [Text]
    splitOn' = \case 
        ""   -> []
        text -> splitOn "," text
