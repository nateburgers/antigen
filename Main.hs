{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Main where
import Prelude as P hiding (FilePath)
import Data hiding (patch)
import Parse
import Text.XML.HXT.Core hiding (trace, (+++))
import Data.List (group)
import Data.Char
import Data.Maybe
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Maybe
import Network.HTTP
import Network.URI
import Shelly
import Debug.Trace
import qualified Data.Text.Lazy as LT
default (LT.Text)

(+++) :: LT.Text -> LT.Text -> LT.Text
a +++ b = LT.append a b
infixr 9 +++

text :: Show a => a -> LT.Text
text = LT.pack . show

rtemsRoot = "http://www.rtems.com/ftp/pub/rtems/SOURCES"

rtemsPage s = LT.concat [rtemsRoot, "/", s, "/"]
rtemsRoute subdirs = LT.intercalate "/" (rtemsRoot:subdirs)

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

-- Shelly script generation
data RtemsConf = RtemsConf
               { version :: Version
               , packages :: [(Package, Maybe Package)]
               } deriving Show

confSources :: RtemsConf -> [Package]
confSources = (map fst) . packages

confDiffs :: RtemsConf -> [Package]
confDiffs = (compactMap snd) . packages

packageRoute v p = rtemsRoute [text v, text p]

pathFor :: [LT.Text] -> LT.Text
pathFor = LT.intercalate "/"

buildDir v = pathFor ["build", text v]

-- configureGeneralPackageRule v p = configurePackageRule v p [ "--target", target
--                                                            , "--prefix", prefix]

-- configureGCCRule :: Version -> Package -> Rules ()
-- configureGCCRule v p = configurePackageRule v p
--                        [ "--build", gccHost
--                        , "--host",  gccHost
--                        , "--target", target
--                        , "--prefix", prefix
--                        , "--verbose"
--                        , "--without-headers"
--                        , "--disable-nls"
--                        , "--enable-languages=c,c++"
--                        ]

-- GCC: make all-gcc, install-gcc, all-libgcc, install-libgcc

-- these should be config options
target  = "powerpc-rtems4.11-rtems"
prefix  = "/home/nate/rtems"
compileHost = "x86_64-unknown-linux-gnu/4.8.1"

type PackageTitle = String
findPackage :: RtemsConf -> PackageTitle -> Package
findPackage conf title = grabPackage $ head $ filter matchBinutils $ packages conf
    where matchBinutils ((Source (Title title') _ _), _) = title' == title
          grabPackage (p, d) = p

packageTitle :: Package -> LT.Text
packageTitle (Source (Title t) _ _) = LT.pack t
packageTitle (Diff (Title t) _ _ _) = LT.pack t
packageTitle (RepoConf _ _ _ _) = LT.pack "rtems"

packageFolderName :: Package -> LT.Text
packageFolderName (Source t v tar) = LT.concat [text t, "-", text v]
packageFolderName (RepoConf v b bv t) = LT.concat ["rtems-", text v, "-repo-conf-0.", text b, ".", text bv]
packageFolderName _ = ""

packageVersion :: Package -> Version
packageVersion (Source _ v _) = v
packageVersion (Diff _ v _ _) = v
packageVersion (RepoConf _ _ bv t) = bv

packageVersionMatches :: Package -> Version -> Bool
packageVersionMatches p v = packageVersion p == v

pairSourceWithDiff :: [Package] -> Package -> (Package, Maybe Package)
pairSourceWithDiff diffs source  = (source, maybeHead $ filter (match source) diffs)
    where match :: Package -> Package -> Bool
          match (Source (Title ts) (Version vs) _)
                    (Diff (Title td) (Version vd) _ _) = ts == td && vs == vd
          match _ _ = False

filterDesiredPackages :: [(Package, Maybe Package)] -> [String] -> [(Package, Maybe Package)]
filterDesiredPackages packages strings = filter (titleIn strings) packages
    where titleIn strings ((Source (Title t) _ _), _) = or $ map (== t) strings

confWithDesiredPackages :: RtemsConf -> [String] -> RtemsConf
confWithDesiredPackages (RtemsConf version packages) titleList =
    RtemsConf version $ filterDesiredPackages packages titleList

scrapeRtemsVersion :: Version -> IO RtemsConf
scrapeRtemsVersion v = do
  packageLinks <- scrape . LT.unpack . rtemsPage . text $ v
  let sources = maxOfJustGroups readPackage packageLinks
      diffs = compactMap readDiff packageLinks
      pairs = map (pairSourceWithDiff diffs) sources
  return $ RtemsConf v pairs

scrapeRtemsRepo :: IO [RtemsConf]
scrapeRtemsRepo = do
  links <- scrape $ LT.unpack $ rtemsPage "/"
  let versions = extractVersionLinks links
  mapM scrapeRtemsVersion versions
         
traceShow' :: Show a => a -> a
traceShow' a = traceShow a a

-- running the scripts
stringPath :: String -> FilePath
stringPath = fromText . LT.pack

showPath :: Show a => a -> FilePath
showPath = stringPath . show

packageBuildDirText :: Version -> Package -> LT.Text
packageBuildDirText v p = (buildDir v) +++ "/" +++ (packageFolderName p +++ "-build")

type GroupRouter = RtemsConf -> FilePath
type PackageRouter = Version -> Package -> FilePath

packageDir :: PackageRouter
packageDir v p = (fromText $ buildDir v) </> (fromText $ packageFolderName p)

packageBuildDir :: PackageRouter
packageBuildDir v p = (fromText $ buildDir v) </> (fromText $ (packageFolderName p `LT.append` "-build"))

type GroupOperation = RtemsConf -> ShIO ()
type PackageOperation = Version -> Package -> ShIO ()
type PairOperation = Version -> (Package, Maybe Package) -> ShIO ()

chBuildDir :: Version -> ShIO () -- Should be a group operation that takes place in side an *all function
chBuildDir = cd . fromText . buildDir

chPackageDir :: PackageOperation
chPackageDir v p = cd $ packageDir v p

chPackageBuildDir :: PackageOperation
chPackageBuildDir v p = cd $ packageBuildDir v p

vshell = shelly . verbosely

curl :: PackageOperation
curl v p = vshell $ do
             chBuildDir v
             downloaded <- test_f $ showPath p
             if downloaded
             then return ()
             else run_ "curl" ["-o", text p, packageRoute v p]

curlAll :: GroupOperation
curlAll (RtemsConf version packages) =
    vshell $ do
      mapM_ curlBoth packages
        where curlBoth (package, possibleDiff) = do
                curl version package
                case possibleDiff of
                  Just diff -> curl version diff
                  Nothing -> return ()

untar :: PackageOperation
untar version package =
    vshell $ do
      chBuildDir version
      untarred <- test_d $ fromText $ packageFolderName package
      if untarred
      then return ()
      else run_ "tar" ["xf", text package]

untarAll :: GroupOperation
untarAll (RtemsConf version packages) = vshell $ mapM_ (untar version . fst) packages

patch :: PairOperation
patch version (package, possibleDiff) =
    vshell $ do
      case possibleDiff of
        Just diff -> do
            chPackageDir version package
            patched <- test_f $ fromText patchStamp
            if patched
            then return ()
            else do
              run_ "patch" ["-i", "../" `LT.append` text diff, "-p1"]
              run_ "touch" [patchStamp]
             where patchStamp = "patchStamp"
        Nothing -> return ()

patchAll :: GroupOperation
patchAll (RtemsConf version packages) = vshell $ mapM_ (patch version) packages

type TextTitle = LT.Text
globalConfigurationOptions :: [LT.Text]
globalConfigurationOptions = [ "--prefix", prefix
                             , "--target", target
                             , "--verbose"
                             ]
configurationOptions :: TextTitle -> [LT.Text]
configurationOptions "gcc" = [ "--with-newlib"
                             , "--enable-threads"
                             , "--enable-languages=c,c++"
                             -- , "--with-gmp=../gmp-4.3.2"
                             -- , "--with-mpfr=../mpfr-2.4.2"
                             -- , "--with-mpc=../mpc-0.8.1"
                             , "--host", compileHost
                             ]
configurationOptions _ = []

configure :: PackageOperation
configure version package =
    vshell $ do
      run_ "mkdir" ["-p", packageBuildDirText version package]
      chPackageBuildDir version package
      makefileGenerated <- test_f $ fromText "Makefile"
      if makefileGenerated
      then return ()
      else run_ (fromText configureScriptPath) configureOptions
          where configureScriptPath = "../" +++ (packageFolderName package) +++ "/configure"
                configureOptions = globalConfigurationOptions ++
                                   (configurationOptions $ packageTitle package)

configureAll :: GroupOperation
configureAll (RtemsConf version packages) = vshell $ mapM_ (configure version . fst) packages



linkTo :: Version -> Package -> Package -> ShIO ()
linkTo version mainPackage linkPackage =
    vshell $ do
      chPackageDir version mainPackage
      linked <- test_d . fromText $ packageTitle linkPackage 
      if linked
      then return ()
      else run_ "ln" ["-s", linkPath, packageTitle linkPackage]
          where linkPath = "../" +++ (packageFolderName linkPackage)

desiredPackages :: [String]
desiredPackages = ["binutils", "gcc", "gdb", "newlib", "gmp", "mpc", "mpfr"]
compiledPackages :: [String]
compiledPackages = ["binutils", "gcc", "gdb"]

mkDirGuard :: LT.Text -> ShIO ()
mkDirGuard directory = vshell $ do
                         dirMade <- test_d $ fromText directory
                         if dirMade
                         then return ()
                         else run_ "mkdir" ["-p", directory]

main :: IO ()
main = do
  configurations <- scrapeRtemsRepo
  let conf = confWithDesiredPackages (last configurations) desiredPackages
      find = findPackage conf
      link = linkTo $ version conf
      binutils = find "binutils"
      gcc = find "gcc"
      gdb = find "gdb"
      newlib = find "newlib"
      gmp = find "gmp"
      mpc = find "mpc"
      mpfr = find "mpfr"
  shelly . verbosely $ do
         --run_ "mkdir" ["-p", buildDir $ version conf]
         mkDirGuard . buildDir $ version conf
         curlAll conf
         untarAll conf
         patchAll conf
         mapM_ ((link gcc) . find) ["gmp", "mpfr", "mpc"]
         configureAll $ confWithDesiredPackages conf compiledPackages
         
