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

-- getting the sources from the repo
packageBuildPath :: Version -> Package -> FilePath
packageBuildPath v p = buildPath v $ show p

packageBuildDir :: Version -> Package -> FilePath
packageBuildDir v p = buildPath v $ packageFolderName p

untar :: Package -> String
untar (Source _ _ GZ) = "-xf"
untar (Source _ _ XZ) = "-xf"
untar (Source _ _ BZ2) = "-xf"
untar _ = error "Untaring something that it shouldn't"

curlPackageRule :: Version -> Package -> Rules ()
curlPackageRule v p = (packageBuildPath v p) *> curlPackage
    where curlPackage name = do
            need [pathFor ["build", show v]]
            system' "curl" ["-o", name, packageRoute v p]

patchPackageRule :: Version -> (Package, Maybe Package) -> Rules ()
patchPackageRule v (p, d) = (packageBuildPath v p) *> curlPackage
    where curlPackage name = do
            case d of
              Nothing ->
                  need [pathFor ["build", show v]]
              Just diff ->
                  need [ pathFor ["build", show v]
                       , packageBuildPath v diff]
            system' "curl" ["-o", name, packageBuildPath v p]
            case d of
              Nothing -> return ()
              Just diff ->
                  system' "patch" [packageBuildDir v p, packageBuildPath v diff]

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
                     sources = confSources conf
                     diffs = confDiffs conf
                 mkDirRules [pathFor ["build", show v]]
                 forM_ sources $ curlPackageRule v
                 --forM_ (packages conf) $ patchPackageRule v
                 forM_ diffs $ curlPackageRule v
                 want $ map (packageBuildPath v) diffs
                 want $ map (packageBuildPath v) sources

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
