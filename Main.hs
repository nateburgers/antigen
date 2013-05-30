module Main where
--import Text.ParserCombinators.Parsec
import Text.Regex.Posix
import Text.XML.HXT.Core
import Data.List
import Data.List.Split
--import Data.Maybe
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Maybe
--import Control.Applicative hiding ((<|>))
import Network.HTTP
import Network.URI

type Title = String
type Version = [Int]
type BuildDate = [Int]
data TarFormat = BZ2 | GZ | XZ | RAW deriving Show
data Package = Source Title Version TarFormat
             | Diff Title Version BuildDate
               deriving Show

titleRegex = "([a-zA-Z+]+-)+([a-zA-Z])*"
versionRegex = "([0-9]+.)*([0-9]+.)"
tarRegex = "(.tar.gz|.tar.xz|.tar.bz2)"
buildDateRegex = "" -- TODO: start work here tomorrow
diffRegex = ".diff"

isDiff :: String -> Bool
isDiff s = s =~ diffRegex :: Bool

trimRegexMatch :: String -> String -> String
trimRegexMatch r s = init ( s =~ r :: String)

matchTitle :: String -> String
matchTitle = trimRegexMatch titleRegex

matchVersion :: String -> String             
matchVersion = trimRegexMatch versionRegex

matchTar :: String -> String
matchTar s = s =~ tarRegex :: String

readTitle :: String -> Title
readTitle = matchTitle

readVersion :: String -> Version
readVersion s = map rd $ splitOn "." $ matchVersion s :: Version
    where rd = read :: String -> Int

readTar :: String -> TarFormat
readTar = makeTar . matchTar
    where makeTar ".tar.bz2" = BZ2
          makeTar ".tar.gz" = GZ
          makeTar ".tar.xz" = XZ
          makeTar _ = RAW
            
readPackage :: String -> Package
readPackage s = if isDiff s
                then Source (readTitle s) (readVersion s) (readTar s)
                else Diff (readTitle s) (readVersion s) [12345567]

testPackage = "gcc-g++-1.2.2.2.3.tar.gz"
test = readPackage testPackage

