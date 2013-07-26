{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Main where
import Prelude as P hiding (FilePath)
import Data
import Parse
import Text.XML.HXT.Core hiding (trace)
import Data.List (group)
import Data.Char
import Data.Maybe
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Maybe
import Network.HTTP
import Network.URI
--import Development.Shake
import Shelly
import Debug.Trace
import qualified Data.Text.Lazy as LT
default (LT.Text)

text :: Show a => a -> LT.Text
text = LT.pack . show

rtemsRoot = "http://www.rtems.com/ftp/pub/rtems/SOURCES"

--rtemsPage :: String -> String
--rtemsPage s = rtemsRoot ++ "/" ++ s ++ "/"
rtemsPage s = LT.concat [rtemsRoot, "/", s, "/"]

--rtemsRoute :: [String] -> String
rtemsRoute subdirs = LT.intercalate "/" (rtemsRoot:subdirs)

--openURL :: String -> MaybeT IO String
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

--packageRoute :: Version -> Package -> String
packageRoute v p = rtemsRoute [text v, text p]

-- Build System
--pathFor :: [String] -> String
pathFor = LT.intercalate "/"

--buildPath :: Version -> String -> String
buildPath v f = pathFor ["build", text v, f]

--quote :: String -> String
quote s = "\"" ++ s ++ "\""

-- getting the sources from the repo
--packageBuildPath :: Version -> Package -> String
packageBuildPath v p = buildPath v $ text p

--buildDir :: Version -> String
buildDir v = pathFor ["build", text v]
--packageBuildDir :: Version -> Package -> String
packageBuildDir v p = buildPath v $ packageFolderName p
--packageConfigureDir :: Version -> Package -> String
packageConfigureDir v p = pathFor ["build", text v, LT.append (packageFolderName p) "-build"]

-- tarPackagePath v p = buildPath v $ show p
-- curlPackageRule :: Version -> Package -> Rules ()
-- curlPackageRule v p = (tarPackagePath v p) *> curlPackage
--     where curlPackage name = do
--             need [pathFor ["build", show v]]
--             system' "curl" ["-o", name, packageRoute v p]

-- configurePackagePath v p = pathFor [packageBuildDir v p, "configure"]
-- untarPackageRule :: Version -> Package -> Rules ()
-- untarPackageRule v p = (configurePackagePath v p) *> \name -> do
--                          need [packageBuildPath v p]
--                          command_ [Cwd $ buildDir v] "tar" ["-xf", show p]

-- changelistPackagePath v p = pathFor [packageProduct v p, "ChangeLog"]
-- patchPackageRule :: Version -> (Package, Maybe Package) -> Rules ()
-- patchPackageRule v (p, Nothing) = (changelistPackagePath v p) *> \name -> do
--                                     need [configurePackagePath v p]
--                                     command_ [Cwd $ buildDir v] "touch" ["ChangeLog"]
-- patchPackageRule v (p, Just diff) = (changelistPackagePath v p) *> \name -> do
--                                       need [ configurePackagePath v p
--                                            , packageBuildPath v diff]
--                                       command_ [Cwd $ packageBuildDir v p] "patch" ["-i", patchFilePath, "-p1"]
--                                           where patchFilePath = pathFor ["..", show diff]

-- makefilePackagePath v p = pathFor [packageConfigureDir v p, "Makefile"]
-- configurePackageRule :: Version -> Package -> [ConfigureOption] -> Rules ()
-- configurePackageRule v p cs = (makefilePackagePath v p) *> \name -> do
--                                 need [changelistPackagePath v p]
--                                 let configureDir = packageConfigureDir v p
--                                     systemWD = command_ [Cwd configureDir]
--                                     configureScriptPath = pathFor ["..", packageFolderName p, "configure"]
--                                 system' "mkdir" ["-p", configureDir]
--                                 systemWD (traceShow configureDir configureScriptPath) cs

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

-- these should be config options
target  = "powerpc-rtems4.11-rtems"
prefix  = "/home/nate/rtems"
--prefix = "/Users/nate/Desktop/rtems"
gccHost = "x86_64-unknown-linux-gnu/4.8.1"

--type ConfigureOption = String
-- compileRule :: Version -> Package -> [ConfigureOption] -> Action ()
-- compileRule v p cs = do
--   let 
--       buildDir = (packageBuildDir v p) ++ "-build"
--       systemPD = command_ [Cwd $ buildDir, Env [("LD_LIBRARY_PATH",prefix++"/lib gmake")]]
--       systemBD = command_ [Cwd $ packageBuildDir v p]
--       configureScriptPath = "../" ++ (packageFolderName p) ++ "/configure"
--   need [makefilePackagePath v p]
--   system' "mkdir" ["-p", buildDir]
--   -- systemPD configureScriptPath $ cs ++ [ "--target", target
--   --                                      , "--prefix", prefix]
--   --systemPD configureScriptPath cs
--   systemPD "printenv" []
--   systemPD "make" ["all"]
--   systemPD "make" ["info"]
--   systemPD "sudo" ["make", "install"]

-- gmpRule :: Version -> Package -> Rules ()
-- gmpRule v p = (packageProduct v p) *> \name -> do
--                 compileRule v p [ "--build", gccHost
--                                 , "--host", target
-- --                                , "--target", target
--                                 , "--enable-shared"
--                                 , "--enable-static"
--                                 , "--enable-fft"]
-- --                                , "--enable-cxx"

-- mpfrRule :: Version -> Package -> Rules ()
-- mpfrRule v p = (packageProduct v p) *> \name -> do
--                  compileRule v p [ "--with-gnu-ld", prefix++"/bin/"++target++"-ld"
--                                  , "--with-gmp", prefix
--                                  , "--enable-static"
--                                  , "--enable-shared"]

-- mpcRule :: Version -> Package -> Rules ()
-- mpcRule v p = (packageProduct v p) *> \name -> do
--                 compileRule v p [ "--with-gnu-ld", prefix++"/bin/"++target++"-ld"
--                                 , "--with-gmp", prefix
--                                 , "--with-mpfr", prefix
--                                 , "--enable-static"
--                                 , "--enable-shared"]

-- newlibRule :: Version -> Package -> Rules ()
-- newlibRule v p = (packageProduct v p) *> \name -> do
--                    compileRule v p []

-- gdbRule :: Version -> Package -> Rules ()
-- gdbRule v p = (packageProduct v p) *> \name -> do
--                 compileRule v p []

-- binutilsRule :: Version -> Package -> Rules ()
-- binutilsRule v p = (packageProduct v p) *> \name -> do
--                      need [makefilePackagePath v p]
--                      let systemWD = command_ [Cwd $ packageConfigureDir v p]
--                          configureScriptPath = pathFor ["..", packageFolderName p, "configure"]
--                      systemWD "make" ["all"]
--                      systemWD "make" ["info"]
--                      systemWD "make" ["install"]

-- gccRule :: Version -> Package -> Rules ()
-- gccRule v p = (packageProduct v p) *> \name -> do
--                 need [makefilePackagePath v p]
--                 let systemWD = command_ [Cwd $ packageConfigureDir v p]
--                     configureScriptPath = pathFor ["..", packageFolderName p, "configure"]
--                 systemWD "make" ["all-gcc"]
--                 systemWD "make" ["all-target-libgcc"]
--                 systemWD "make" ["install-gcc"]
--                 systemWD "make" ["install-target-libgcc"]


-- mkDirRule :: FilePath -> Rules ()
-- mkDirRule d = d *> mkdir
--     where mkdir d = system' "mkdir" ["-p", d]
-- mkDirRules :: [FilePath] -> Rules ()
-- mkDirRules ds = forM_ ds mkDirRule

packageDir v p = pathFor ["build", text v] -- START WORK HERE

-- packageProduct :: Version -> Package -> FilePath
-- packageProduct v p = pathFor [packageBuildDir v p, title p]
--     where title (Source (Title t) _ _) = t
--           title (Diff (Title t) _ _ _) = t
--           title (RepoConf _ _ _ _) = "rtems"

-- may crash if not found, should return a maybe
type PackageTitle = String
findPackage :: RtemsConf -> PackageTitle -> Package
findPackage conf title = grabPackage $ head $ filter matchBinutils $ packages conf
    where matchBinutils ((Source (Title title') _ _), _) = title' == title
          grabPackage (p, d) = p

-- shakeIt :: RtemsConf -> IO ()
-- shakeIt conf = shakeArgs shakeOptions $ do
--                  let v = version conf
--                      diffs = confDiffs conf
--                      packagesWithPatches = packages conf
--                      binutils = findPackage conf "binutils"
--                      gcc = findPackage conf "gcc"
--                      gdb = findPackage conf "gdb"
--                      newlib = findPackage conf "newlib"
--                      gmp = findPackage conf "gmp"
--                      mpfr = findPackage conf "mpfr"
--                      mpc = findPackage conf "mpc"
--                  mkDirRules [pathFor ["build", show v]]
--                  forM_ packagesWithPatches $ patchPackageRule v
--                  forM_ diffs $ curlPackageRule v
--                  -- individual package rules
--                  --                 newlibRule v newlib >> want [packageProduct v newlib]
--                  curlPackageRule v gcc
--                  untarPackageRule v gcc
--                  configureGCCRule v gcc
--                  gccRule v gcc
--                  want [packageProduct v gcc]
--                  -- mpcRule v mpc >> want [packageProduct v mpc]
--                  -- mpfrRule v mpfr >> want [packageProduct v mpfr]
--                  -- gmpRule v gmp >> want [packageProduct v gmp]
--                  curlPackageRule v binutils
--                  untarPackageRule v binutils
--                  configureGeneralPackageRule v binutils
--                  binutilsRule v binutils
--                  want [packageProduct v binutils]
-- --                 gdbRule v gdb >> want [packageProduct v gdb]

--packageFolderName :: Package -> String
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

chVersionDir :: Version -> ShIO ()
chVersionDir = cd . fromText . buildDir

chBuildDir :: RtemsConf -> ShIO ()
chBuildDir = chVersionDir . version

vshell = shelly . verbosely

type GroupOperation = RtemsConf -> ShIO ()
type PackageOperation = Version -> Package -> ShIO ()

curl :: PackageOperation
curl v p = vshell $ do
             chVersionDir v
             downloaded <- test_f $ showPath p
             if not downloaded
             then run_ "curl" ["-o", text p, packageRoute v p]
             else return ()

curlAll :: GroupOperation
curlAll (RtemsConf v ps) = vshell $ do
                             mapM_ curlBoth ps
                                 where curlBoth (package, possibleDiff) = do
                                         curl v package
                                         case possibleDiff of
                                           Just diff -> curl v diff
                                           Nothing -> return ()

-- patch :: (Package, Maybe Package) -> ShIO ()
-- patch (package, possibleDiff) = shelly $ verbosely $ do
--                                   case possibleDiff of
--                                     Just diff ->
                                        
--                                     Nothing -> return ()

untar :: PackageOperation
untar v p = vshell $ do
              chVersionDir v
              run_ "tar" ["xf", text p]

untarAll :: GroupOperation
untarAll (RtemsConf v ps)  = vshell $ mapM_ (untar v . fst) ps

-- patch :: Package -> Package -> ShIO ()
-- patch package diff = shelly $ verbosely $ do
                       

main :: IO ()
main = do
  configurations <- scrapeRtemsRepo
  let conf = last configurations
  shelly . verbosely $ do
         run_ "mkdir" ["-p", buildDir $ version conf]
         curlAll conf
         untarAll conf

-- main :: IO ()
-- main = do
--   configuration <- scrapeRtemsRepo
--   putStrLn $ show $ last configuration

-- main = shakeIt . traceShow' . last =<< scrapeRtemsRepo
-- main' = do
--   configurations <- scrapeRtemsRepo
--   forM_ configurations shakeIt
