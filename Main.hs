module Main where
import Text.ParserCombinators.Parsec
import Text.Regex.Posix
import Text.XML.HXT.Core
import Data.List
--import Data.List.Split
import Data.Maybe
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Maybe
import Control.Applicative hiding ((<|>))
import Network.HTTP
import Network.URI

data Title = Title String
data Version = Version [Int]
data BuildDate = BuildDate [Int] deriving Show
data TarFormat = BZ2 | GZ | XZ
data Package = Source Title Version TarFormat
             | Diff Title Version BuildDate
               deriving Show

-- helpers
class Token a where
    tokenize :: a -> String
    genParser :: a -> GenParser Char st a
    genParser t = (string $ tokenize t) >> return t

instance Token TarFormat where
    tokenize GZ = "gz"
    tokenize XZ = "xz"
    tokenize BZ2 = "bz2"

instance Show TarFormat where
    show t = ".tar." ++ tokenize t

instance Show Version where
    show (Version v) = foldr accumShow "" v
        where accumShow x xs = (show x)++xs

instance Show Title where
    show (Title t) = t

-- real parsing
dash :: GenParser Char st Char
dash = char '-'

dot :: GenParser Char st Char
dot = char '.'

titleLetter :: GenParser Char st Char
titleLetter = oneOf $ "+"++['a'..'z']++['A'..'Z']

letters :: GenParser Char st String
letters = many1 titleLetter

digits :: GenParser Char st String
digits = many1 digit

titleHead = letters
titleTail = do
  head <- dash
  tail <- letters
  return $ head:tail        

titleParser :: GenParser Char st Title
titleParser = do
  head <- titleHead
  tail <- many1 $ try titleTail
  return $ Title $ strCat $ head:tail

versionHead = digits
versionTail = dot >> digits

versionParser :: GenParser Char st Version
versionParser = do
  head <- versionHead
  tail <- many1 $ try versionTail
  return $ Version $ map readInt $ head:tail

tarHead = string "tar"
tarTail = genParser GZ <|>
          genParser XZ <|>
          genParser BZ2

parseTar :: GenParser Char st TarFormat
parseTar = dot >> tarHead >> dot >> tarTail

packageParser = do
  title <- titleParser
  dash
  version <- versionParser
  tar <- parseTar
  return (title, version, tar)

strCat = foldr (++) []
readInt :: String -> Int
readInt = read

parse' p = parse p "error"
parseT = parse' titleParser
parseV = parse' versionParser

testT = parseT "some-title-dude-man-bro"
testV = parseV "1231.132.123.12333.3.3.3.3"
testP = parse' packageParser "gcc-g++-4.1.2.2.2.tar.gz"

--

-- titleRegex = "([a-zA-Z+]+-)+([a-zA-Z])*"
-- versionRegex = "([0-9]+.)*([0-9]+.)"
-- tarRegex = "(.tar.gz|.tar.xz|.tar.bz2)"
-- buildDateRegex = "" -- TODO: start work here tomorrow
-- diffRegex = ".diff"

-- isDiff :: String -> Bool
-- isDiff s = s =~ diffRegex :: Bool

-- trimRegexMatch :: String -> String -> String
-- trimRegexMatch r s = init ( s =~ r :: String)

-- matchTitle :: String -> String
-- matchTitle = trimRegexMatch titleRegex

-- matchVersion :: String -> String             
-- matchVersion = trimRegexMatch versionRegex

-- matchTar :: String -> String
-- matchTar s = s =~ tarRegex :: String

-- readTitle :: String -> Title
-- readTitle = matchTitle

-- readVersion :: String -> Version
-- readVersion s = map rd $ splitOn "." $ matchVersion s :: Version
--     where rd = read :: String -> Int

-- readTar :: String -> TarFormat
-- readTar = makeTar . matchTar
--     where makeTar ".tar.bz2" = BZ2
--           makeTar ".tar.gz" = GZ
--           makeTar ".tar.xz" = XZ
--           makeTar _ = RAW
            
-- readPackage :: String -> Package
-- readPackage s = if isDiff s
--                 then Source (readTitle s) (readVersion s) (readTar s)
--                 else Diff (readTitle s) (readVersion s) [12345567]

-- testPackage = "gcc-g++-1.2.2.2.3.tar.gz"
-- test = readPackage testPackage

