{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}

module Main where
import Data
import Parse
import Text.XML.HXT.Core hiding (trace)
import Data.List
import Data.Maybe
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Maybe
import Network.HTTP
import Network.URI
import Development.Shake
import Debug.Trace

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
          diffMatches (Source (Title sourceTitle) (Version sourceVersion) _)
                          (Diff (Title diffTitle) (Version diffVersion) _ _) =
              sourceTitle == diffTitle && sourceVersion == diffVersion
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

quote :: String -> String
quote s = "\"" ++ s ++ "\""

-- getting the sources from the repo
packageBuildPath :: Version -> Package -> FilePath
packageBuildPath v p = buildPath v $ show p

packageBuildDir :: Version -> Package -> FilePath
packageBuildDir v p = buildPath v $ packageFolderName p

curlPackageRule :: Version -> Package -> Rules ()
curlPackageRule v p = (packageBuildPath v p) *> curlPackage
    where curlPackage name = do
            need [pathFor ["build", show v]]
            system' "curl" ["-o", name, packageRoute v p]

patchPackageRule :: Version -> (Package, Maybe Package) -> Rules ()
patchPackageRule v (p, d) = (packageBuildPath v p) *> curlPackage
    where curlPackage name = do
            let buildDir = pathFor ["build", show v]
                packageDir = packageBuildDir v p
                curl = system' "curl" ["-o", name, packageRoute v p]
                untar = systemCwd buildDir "tar" ["-xf", show p]
            case d of
              Nothing -> do
                     need [buildDir]
                     curl >> untar
                     return ()
              Just diff -> do
                     need [buildDir, packageBuildPath v diff]
                     curl >> untar
                     systemCwd packageDir "patch" ["-i", patchFilePath, "-p1"]
                         where patchFilePath = pathFor ["..", show diff]

-- may crash if not found, should return a maybe
type PackageTitle = String
findPackage :: RtemsConf -> PackageTitle -> Package
findPackage conf title = grabPackage $ head $ filter matchBinutils $ packages conf
    where matchBinutils ((Source (Title title') _ _), _) = title' == title
          grabPackage (p, d) = p

-- these should be config options
target  = "powerpc-rtems4.11"
prefix  = "/home/nate/rtems"
gccHost = "x86_64-unknown-linux-gnu/4.8.1"

type ConfigureOption = String
compileRule :: Version -> Package -> [ConfigureOption] -> Rules ()
compileRule v p cs = packageProduct v p *> compile
    where systemPD = systemCwd $ packageBuildDir v p
          systemIberty = systemCwd $ pathFor [packageBuildDir v p, "libiberty"]
          compile _ = do
            need [packageBuildPath v p]
            path <- getEnv "PATH"
            traced "looking up PATH for" $ do
                        case path of
                          Nothing -> putStrLn "Nothing"
                          Just s -> putStrLn s
            systemPD "./configure" $ cs ++ [ "--target", target
                                           , "--prefix", prefix]
            systemPD "make" ["all"]
            systemPD "make" ["info"]
            systemPD "sudo" ["make", "install"]

gdbRule :: Version -> Package -> Rules ()
gdbRule v p = compileRule v p []

binutilsRule :: Version -> Package -> Rules ()
binutilsRule v p = compileRule v p []

gccRule :: Version -> Package -> Rules ()
gccRule v p = compileRule v p
              [ "--build", gccHost
              , "--host",  gccHost
              , "--with-gnu-as", prefix++"/bin/"++target++"-as"
              , "--with-gnu-ld", prefix++"/bin/"++target++"-ld"
              , "--with-ar", prefix++"/bin/"++target++"-ar"
              , "--verbose"
              , "--enable-threads"
              , "--disable-install-libiberty"
              , "--enable-languages=c"]

mkDirRule :: FilePath -> Rules ()
mkDirRule d = d *> mkdir
    where mkdir d = system' "mkdir" ["-p", d]
mkDirRules :: [FilePath] -> Rules ()
mkDirRules ds = forM_ ds mkDirRule

packageDir :: Version -> Package -> FilePath
packageDir v p = pathFor ["build", show v] -- START WORK HERE

packageProduct :: Version -> Package -> FilePath
packageProduct v p = pathFor [packageBuildDir v p, title p]
    where title (Source (Title t) _ _) = t
          title (Diff (Title t) _ _ _) = t
          title (RepoConf _ _ _ _) = "rtems"

shakeIt :: RtemsConf -> IO ()
shakeIt conf = shakeArgs shakeOptions $ do
                 let v = version conf
--                     sources = confSources conf
                     diffs = confDiffs conf
                     packagesWithPatches = packages conf
                     binutils = findPackage conf "binutils"
                     gcc = findPackage conf "gcc"
                     gdb = findPackage conf "gdb"
                 mkDirRules [pathFor ["build", show v]]
                 forM_ packagesWithPatches $ patchPackageRule v
                 forM_ diffs $ curlPackageRule v
                 -- individual package rules
                 binutilsRule v binutils >> want [packageProduct v binutils]
                 gccRule v gcc >> want [packageProduct v gcc]
                 gdbRule v gdb >> want [packageProduct v gdb]

pairSourceWithDiff :: [Package] -> Package -> (Package, Maybe Package)
pairSourceWithDiff diffs source  = (source, maybeHead $ filter (match source) diffs)
    where match :: Package -> Package -> Bool
          match (Source (Title ts) (Version vs) _)
                    (Diff (Title td) (Version vd) _ _) = ts == td && vs == vd
          match _ _ = False

scrapeRtemsVersion :: Version -> IO RtemsConf
scrapeRtemsVersion v = do
  packageLinks <- scrape . rtemsPage . show $ v
  let sources = maxOfJustGroups readPackage packageLinks
      diffs = compactMap readDiff packageLinks
      pairs = map (pairSourceWithDiff diffs) sources
  return $ RtemsConf v pairs

scrapeRtemsRepo :: IO [RtemsConf]
scrapeRtemsRepo = do
  links <- scrape $ rtemsPage "/"
  let versions = extractVersionLinks links
  mapM scrapeRtemsVersion versions
         
traceShow' :: Show a => a -> a
traceShow' a = traceShow a a

main = shakeIt . traceShow' . last =<< scrapeRtemsRepo
main' = do
  configurations <- scrapeRtemsRepo
  forM_ configurations shakeIt
