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
        where accumShow x "" = show x
              accumShow x xs = (show x)++"."++xs

instance Show Title where
    show (Title t) = t

instance Show Package where
    show (Source t v f) = (show t) ++ "-" ++ (show v) ++ (show f)
    show (Diff t v b) = "diff"

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

tarParser :: GenParser Char st TarFormat
tarParser = dot >> tarHead >> dot >> tarTail

packageParser = do
  title <- titleParser
  dash
  version <- versionParser
  tar <- tarParser
  return $ Source title version tar

strCat = foldr (++) []
readInt :: String -> Int
readInt = read

parse' p = parse p "error"

parseTitle = parse' titleParser
parseVersion = parse' versionParser
parsePackage = parse' packageParser

packageTest = "gcc-g++-4.1123.2.2342.2.tar.gz"

testP = parsePackage packageTest
