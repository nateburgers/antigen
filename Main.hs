{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}

module Main where
import Data
import Parse
import Text.XML.HXT.Core
import Data.List
import Data.Maybe
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Maybe
import Network.HTTP
import Network.URI
import Development.Shake
--import Shelly hiding (get)
import Debug.Trace              -- get rid of this
-- import qualified Data.Text as T
-- default (T.Text)

rtemsRoot = "http://www.rtems.com/ftp/pub/rtems/SOURCES"

rtemsPage :: String -> String
rtemsPage s = rtemsRoot ++ "/" ++ s ++ "/"

rtemsRoute :: [String] -> String -- Technically produces incorrect urls
rtemsRoute subdirs = intercalate "/" (rtemsRoot:subdirs)

openURL :: String -> MaybeT IO String
openURL url = case parseURI url of
                Nothing -> fail "could not read URI"
                Just u -> liftIO (getResponseBody =<< simpleHTTP (mkRequest GET u))

get url = do
  contents <- runMaybeT $ openURL url
  return $ readString [withParseHTML yes, withWarnings no] (fromMaybe "" contents)

css :: ArrowXml a => String -> a XmlTree XmlTree
css = multi . hasName

scrape url = do
  page <- get url
  runX $ page >>> css "td" >>> css "a" >>> getAttrValue "href"

compact :: [Maybe a] -> [a]
compact = foldr compact' []
    where compact' Nothing xs = xs
          compact' (Just x) xs = x:xs

compactMap :: (a -> Maybe b) -> [a] -> [b]
compactMap f xs = compact $ map f xs

extractVersionLinks = compactMap readVersionLink
extractPackages = compactMap readPackage
extractDiffs = compactMap readDiff
extractRepoConfigurations = compactMap readRepoConf

maxOfGroups :: Ord a => [a] -> [a]
maxOfGroups xs = map maximum $ group xs

maxOfJustGroups :: Ord b => (a -> Maybe b) -> [a] -> [b]
maxOfJustGroups f = maxOfGroups . compactMap f

maybeHead :: [a] -> Maybe a
maybeHead (x:_) = Just x
maybeHead [] = Nothing

-- takes in a list of sources and a list of diffs and returns list of a source and maybe diff
packagesWithDiffs :: [Package] -> [Package] -> [(Package, Maybe Package)]
packagesWithDiffs sources diffs = map (findAssoc diffs) sources
    where findAssoc diffs source = (source, maybeHead $ filter (diffMatches source) diffs)
          diffMatches (Source (Title sourceTitle) _ _) (Diff (Title diffTitle) _ _ _) =
              sourceTitle == diffTitle
          diffMatches _ _ = False

-- Shelly script generation
data RtemsConf = RtemsConf
               { version :: Version
               , packages :: [(Package, Maybe Package)]
               } deriving Show

confSources :: RtemsConf -> [Package]
confSources = (map fst) . packages

confDiffs :: RtemsConf -> [Package]
confDiffs = (compactMap snd) . packages

packageRoute :: Version -> Package -> String
packageRoute v p = rtemsRoute [show v, show p]

-- Build System
pathFor :: [FilePath] -> FilePath
pathFor = intercalate "/"

buildPath :: Version -> FilePath -> FilePath
buildPath v f = pathFor ["build", show v, f]

packageBuildPath :: Version -> Package -> FilePath
packageBuildPath v p = buildPath v $ show p

curlPackageRule :: Version -> Package -> Rules ()
curlPackageRule v p = (buildPath v $ show p) *> curlPackage
    where curlPackage name = do
            need [pathFor ["build", show v]]
            system' "curl" ["-o", name, packageRoute v p]

mkDirRule :: FilePath -> Rules ()
mkDirRule d = d *> mkdir
    where mkdir d = system' "mkdir" ["-p", d]
mkDirRules :: [FilePath] -> Rules ()
mkDirRules ds = forM_ ds mkDirRule

packageDir :: Version -> Package -> FilePath
packageDir v p = pathFor ["build", show v] -- START WORK HERE

shakeIt :: RtemsConf -> IO ()
shakeIt conf = shakeArgs shakeOptions $ do
                 let v = version conf
                 mkDirRules [pathFor ["build", show v]]
                 forM_ (confSources conf) $ curlPackageRule v
                 forM_ (confDiffs conf) $ curlPackageRule v
                 want $ map (packageBuildPath v) $ confSources conf
                 want $ map (packageBuildPath v) $ confDiffs conf

scrapeRtemsRepo :: IO [RtemsConf]
scrapeRtemsRepo = do
  links <- scrape $ rtemsPage "/"
  let versions = extractVersionLinks links
  packageLinks <- mapM (scrape . rtemsPage . show) versions 
  let sources = map (maxOfJustGroups readPackage) packageLinks
      diffs = map (maxOfJustGroups readDiff) packageLinks
      confs = map (maxOfJustGroups readRepoConf) packageLinks
      configuration = zipWith RtemsConf versions $ zipWith packagesWithDiffs sources diffs
  return $ configuration
         
main = shakeIt . last =<< scrapeRtemsRepo
main' = do
  configurations <- scrapeRtemsRepo
  forM_ configurations shakeIt
