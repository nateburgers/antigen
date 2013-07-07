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

buildDir v = pathFor ["build", v]
packageBuildDir :: Version -> Package -> FilePath
packageBuildDir v p = buildPath v $ packageFolderName p

-- packageConfigurePath :: Version -> Package -> FilePath
-- packageConfigurePath v p = pathFor [packageBuildDir v p, "configure"]

tarPackagePath v p = buildPath v $ show p
curlPackageRule :: Version -> Package -> Rules ()
curlPackageRule v p = (tarPackagePath v p) *> curlPackage
    where curlPackage name = do
            need [pathFor ["build", show v]]
            system' "curl" ["-o", name, packageRoute v p]

configurePackagePath v p = pathFor [packageBuildDir v p, "configure"]
untarPackageRule :: Version -> Package -> Rules ()
untarPackageRule v p = (configurePackagePath v p) *> \name -> do
                         need [packageBuildPath v p]
                         command_ [Cwd $ buildDir v] "tar" ["-xf", show p]

changelistPackagePath v p = pathFor [packageBuildDir v p, "Changelist.rtems"]
patchPackageRule :: Version -> (Package, Maybe Package) -> Rules ()
patchPackageRule v (p, Nothing) = (changelistPackagePath v p) *> \name -> do
                                    need [configurePackagePath v p]
                                    command_ [Cwd $ buildDir] "touch" ["Changelog.rtems"]
patchPackageRule v (p, Just diff) = (changelistPackagePath v p) *> \name -> do
                                      need [ configurePackagePath v p
                                           , packageBuildPath v diff]
                                      command_ [Cwd $ packageBuildDir v p] "patch" ["-i", patchFilePath, "-p1"]
-- patchPackageRule v (p, d) = (changelistPackagePath v p) *> curlPackage
--     where curlPackage _ = do
--             let buildDir = pathFor ["build", show v]
--                 packageDir = packageBuildDir v p
--                 curl = system' "curl" ["-o", packageBuildPath v p
--                                       , packageRoute v p]
--                 untar = systemCwd buildDir "tar" ["-xf", show p]
--             case d of
--               Nothing -> do
--                      need [buildDir, packageBuildPath v p]
--                      --curl >> untar
--                      untar
--                      return ()
--               Just diff -> do
--                      need [ buildDir, packageBuildPath v diff
--                           , packageBuildPath v p]
--                      untar
--                      systemCwd packageDir "patch" ["-i", patchFilePath, "-p1"]
--                          where patchFilePath = pathFor ["..", show diff]

-- these should be config options
target  = "powerpc-rtems4.11-rtems"
prefix  = "/home/nate/rtems"
gccHost = "x86_64-unknown-linux-gnu/4.8.1"

type ConfigureOption = String
compileRule :: Version -> Package -> [ConfigureOption] -> Action ()
compileRule v p cs = do
  let 
      buildDir = (packageBuildDir v p) ++ "-build"
      systemPD = command_ [Cwd $ buildDir, Env [("LD_LIBRARY_PATH",prefix++"/lib gmake")]]
      systemBD = command_ [Cwd $ packageBuildDir v p]
      configureScriptPath = "../" ++ (packageFolderName p) ++ "/configure"
  need [packageConfigurePath v p]
  system' "mkdir" ["-p", buildDir]
  -- systemPD configureScriptPath $ cs ++ [ "--target", target
  --                                      , "--prefix", prefix]
  systemPD configureScriptPath cs
  systemPD "printenv" []
  systemPD "make" ["all"]
  systemPD "make" ["info"]
  systemPD "sudo" ["make", "install"]

gmpRule :: Version -> Package -> Rules ()
gmpRule v p = (packageProduct v p) *> \name -> do
                compileRule v p [ "--build", gccHost
                                , "--host", target
--                                , "--target", target
                                , "--enable-shared"
                                , "--enable-static"
                                , "--enable-fft"
--                                , "--enable-cxx"
                                , "CC", "gcc"]

mpfrRule :: Version -> Package -> Rules ()
mpfrRule v p = (packageProduct v p) *> \name -> do
                 compileRule v p [ "--with-gnu-ld", prefix++"/bin/"++target++"-ld"
                                 , "--with-gmp", prefix
                                 , "--enable-static"
                                 , "--enable-shared"]

mpcRule :: Version -> Package -> Rules ()
mpcRule v p = (packageProduct v p) *> \name -> do
                compileRule v p [ "--with-gnu-ld", prefix++"/bin/"++target++"-ld"
                                , "--with-gmp", prefix
                                , "--with-mpfr", prefix
                                , "--enable-static"
                                , "--enable-shared"]

newlibRule :: Version -> Package -> Rules ()
newlibRule v p = (packageProduct v p) *> \name -> do
                   compileRule v p []

gdbRule :: Version -> Package -> Rules ()
gdbRule v p = (packageProduct v p) *> \name -> do
                compileRule v p []

binutilsRule :: Version -> Package -> Rules ()
binutilsRule v p = (packageProduct v p) *> \name -> do
                     system' "echo" ["yay"]
                     compileRule v p []

gccRule :: Version -> Package -> Rules ()
gccRule v p = (packageProduct v p) *> \name -> do
                compileRule v p [ "--build", gccHost
                                , "--host",  gccHost
                                , "--target", target
                                , "--prefix", prefix
                                , "--with-gnu-as", prefix++"/bin/"++target++"-as"
                                , "--with-gnu-ld", prefix++"/bin/"++target++"-ld"
--                                , "--with-ar", prefix++"/bin/"++target++"-ar"
                                , "--verbose"
                                , "--without-headers"
                                , "--with-newlib"
                                , "--disable-shared"
                                , "--enable-static"
                                , "--enable-ld"
                                -- , "--with-gmp", prefix
                                -- , "--with-mpc", prefix
                                -- , "--with-mpfr", prefix
--                                , "--disable-libssp"
                                , "--disable-nls"
                                , "--disable-libgomp"
--                                , "--enable-multilib"
--                                , "--enable-threads"
                                , "--enable-languages=c"
                                , "CPP=\"gcc -E\""]

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

-- may crash if not found, should return a maybe
type PackageTitle = String
findPackage :: RtemsConf -> PackageTitle -> Package
findPackage conf title = grabPackage $ head $ filter matchBinutils $ packages conf
    where matchBinutils ((Source (Title title') _ _), _) = title' == title
          grabPackage (p, d) = p

shakeIt :: RtemsConf -> IO ()
shakeIt conf = shakeArgs shakeOptions $ do
                 let v = version conf
                     diffs = confDiffs conf
                     packagesWithPatches = packages conf
                     binutils = findPackage conf "binutils"
                     gcc = findPackage conf "gcc"
                     gdb = findPackage conf "gdb"
                     newlib = findPackage conf "newlib"
                     gmp = findPackage conf "gmp"
                     mpfr = findPackage conf "mpfr"
                     mpc = findPackage conf "mpc"
                 mkDirRules [pathFor ["build", show v]]
                 forM_ packagesWithPatches $ patchPackageRule v
                 forM_ diffs $ curlPackageRule v
                 -- individual package rules
--                 newlibRule v newlib >> want [packageProduct v newlib]
                 gccRule v gcc >> want [packageProduct v gcc]
                 -- mpcRule v mpc >> want [packageProduct v mpc]
                 -- mpfrRule v mpfr >> want [packageProduct v mpfr]
                 -- gmpRule v gmp >> want [packageProduct v gmp]
--                 binutilsRule v binutils >> want [packageProduct v binutils]
--                 gdbRule v gdb >> want [packageProduct v gdb]

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
