{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules, DoAndIfThenElse, DeriveDataTypeable, QuasiQuotes #-}

module Main where
import Prelude as P hiding (FilePath)
import Data hiding (patch)
import Parse
import Text.XML.HXT.Core hiding (trace, (+++))
import Data.List (group)
import Data.Char
import Data.Maybe
import Text.Shakespeare.Text (lt)
import qualified System.Console.CmdLib as CL
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

rtemsRoot :: LT.Text
rtemsRoot = "http://www.rtems.com/ftp/pub/rtems/SOURCES"

rtemsPage :: LT.Text -> LT.Text
rtemsPage s = LT.concat [rtemsRoot, "/", s, "/"]

rtemsRoute :: [LT.Text] -> LT.Text
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
data TargettedConf = TargettedConf
                   { tConfConf :: RtemsConf
                   , tConfTarget :: LT.Text
                   } deriving Show

confSources :: RtemsConf -> [Package]
confSources = (map fst) . packages

confDiffs :: RtemsConf -> [Package]
confDiffs = (compactMap snd) . packages

packageRoute v p = rtemsRoute [text v, text p]

pathFor :: [LT.Text] -> LT.Text
pathFor = LT.intercalate "/"

buildDir v = pathFor ["build", text v]
rtemsDir v = pathFor ["build", text v, "rtems"]
rtemsBuildDir v = pathFor ["build", text v, "rtems-build"]

-- FIXME: evil hard-coded values from lazyness
target  = "arm-rtems4.11-rtems"
prefix  = "~/rtems"
compileHost = "x86_64-linux-gnu/4.6.3"

type PackageTitle = String
findPackage :: RtemsConf -> PackageTitle -> Package
findPackage conf title = grabPackage $ head $ filter matchPackage $ packages conf
    where matchPackage ((Source (Title title') _ _), _) = title' == title
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
                             , "--without-nls"
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

makeTargets :: TextTitle -> [LT.Text]
makeTargets "gcc" = ["all", "info", "install"]
makeTargets "binutils" = ["all", "info", "install"]
makeTargets "gdb" = ["all", "info", "install"]
makeTargets _ = []

make :: PackageOperation
make version package =
    vshell $ do
      chPackageBuildDir version package
      appendToPath . fromText $ prefix +++ "/bin"
      run_ "make" $ ["clean"] ++ (makeTargets . packageTitle $ package)

makeAll :: GroupOperation
makeAll (RtemsConf version packages) = vshell $ mapM_ (make version . fst ) packages

linkTo :: Version -> Package -> Package -> ShIO ()
linkTo version mainPackage linkPackage =
    vshell $ do
      chPackageDir version mainPackage
      linked <- test_d . fromText $ packageTitle linkPackage 
      if linked
      then return ()
      else run_ "ln" ["-s", linkPath, "."]
          where linkPath = "../" +++ (packageFolderName linkPackage) +++ "/" +++ (packageTitle linkPackage) 

rtemsVersionBranch :: Version -> LT.Text
rtemsVersionBranch (Version [4, 11]) = "master"
rtemsVersionBranch (Version [4, 10]) = "4.10"
rtemsVersionBranch (Version [4, 9]) = "4.9"
rtemsVersionBranch _ = "master"

compileRtems :: Version -> ShIO ()
compileRtems version =
    vshell $ do
      chBuildDir version
      appendToPath . fromText $ prefix +++ "/bin"
      appendToPath . fromText $ prefix +++ "/" +++ target +++ "/bin"
      run_ "git" ["clone", "https://github.com/RTEMS/rtems"]
      cd . fromText $ "rtems"
      run_ "git" ["checkout", rtemsVersionBranch version]
      cd . fromText $ ".."
      run_ "rtems/bootstrap" []
      run_ "mkdir" ["-p", "rtems-build"]
      cd . fromText $ "rtems-build"
      run_ "../rtems/configure" [ "--target", target
                                , "--prefix", prefix
                                , "--enable-rtemsbsp=sis"
                                , "--enable-tests=samples"
                                , "--disable-posix"]
      run_ "make" []
      run_ "make" ["install"]

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

build :: RtemsConf -> IO ()
build conf = do
  let find = findPackage conf
      link = linkTo $ version conf
  shelly . verbosely $ do
         mkDirGuard . buildDir $ version conf
         curlAll conf
         untarAll conf
         patchAll conf
         link (find "gcc") (find "newlib")
         configureAll $ confWithDesiredPackages conf compiledPackages
         makeAll $ confWithDesiredPackages conf ["binutils", "gcc", "gdb"]
         compileRtems $ version conf
  
data Options = Options
             { rversion :: String
             , rtarget :: String
             , rprefix :: String
             } deriving (CL.Typeable, CL.Data, Eq)
instance CL.Attributes Options where
    attributes _ = CL.group "Options" [ rversion CL.%> [ CL.Help "RTEMS version to install."
                                                       , CL.ArgHelp "[4.11|4.10|4.9]"
                                                       , CL.Default "4.11" ]
                                      , rtarget CL.%> [ CL.Help "Target platform."
                                                      , CL.ArgHelp "[i386|arm|powerpc...]"
                                                      , CL.Default "i386" ]
                                      , rprefix CL.%> [ CL.Help "Install Location." ]
                                      ]
instance CL.RecordCommand Options where
    mode_summary _ = "Configuration-Less RTEMS Source Builder."

-- FIXME: implicitly cascade case..of Maybe's with a MonadT for an option with error texts
main :: IO ()
main = CL.getArgs >>= CL.executeR Options {} >>= \options -> do
         let v = readParser versionParser $ rversion options
         case v of
           Nothing -> do
                  putStrLn $ LT.unpack [lt|Could not parse version #{rversion options}|]
           Just properVersion -> do
               configurations <- scrapeRtemsRepo
               let versionedConfig = maybeHead $ filter (matchVersion properVersion) configurations
                       where matchVersion v (RtemsConf version _) = version == v 
               case versionedConfig of
                 Nothing -> do
                   putStrLn $ LT.unpack [lt|Could not find an RTEMS configuration with version #{text properVersion}|]
                 Just conf -> build conf
