module Parse where
import Text.ParserCombinators.Parsec
import Control.Applicative hiding ((<|>))
import Data.Char

data Title = Title String
data Version = Version [Int]
data BuildDate = BuildDate [Int]
data TarFormat = BZ2 | GZ | XZ
data Package = Source Title Version TarFormat
             | Diff Title Version Version BuildDate
             | RepoConf Version BuildDate Version TarFormat

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
    show t = "tar." ++ tokenize t

instance Show Version where
    show (Version v) = foldr accumShow "" v
        where accumShow x "" = show x
              accumShow x xs = (show x) ++ "." ++ xs

instance Show BuildDate where
    show (BuildDate ds) = foldr accumShow "" ds
      where accumShow x xs = (show x) ++ xs

instance Show Title where
    show (Title t) = t

instance Show Package where
    show (Source t v f) = foldCat [show t, "-", show v, ".", show f]
    show (Diff t v rv b) = foldCat [show t, "-", show v, "-rtems", show rv, "-", show b, ".diff"]
    show (RepoConf v b bv t) = foldCat ["rtems-", show v, "-repo-conf-0.", show b, ".", show bv, show t]

instance Eq Package where
    (==) (Source (Title ta) _ _) (Source (Title tb) _ _) = ta == tb
instance Ord Package where
    (<=) (Source _ (Version va) _) (Source _ (Version vb) _) = va <= vb

foldCat :: [String] -> String
foldCat = foldr (++) ""

-- real parsing
dash :: GenParser Char st Char
dash = char '-'

dot :: GenParser Char st Char
dot = char '.'

slash :: GenParser Char st Char
slash = char '/'

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

multiTitleParser :: GenParser Char st Title
multiTitleParser = do
  head <- titleHead
  tail <- many1 $ try titleTail
  return $ Title $ strCat $ head:tail

unitTitleParser :: GenParser Char st Title
unitTitleParser = titleHead >>= return . Title

titleParser :: GenParser Char st Title
titleParser = try multiTitleParser <|>
              unitTitleParser

versionHead = digits
versionTail = dot >> digits

multiVersionParser :: GenParser Char st Version
multiVersionParser = do
  head <- versionHead
  tail <- many1 $ try versionTail
  return $ Version $ map readInt $ head:tail

unitVersionParser :: GenParser Char st Version
unitVersionParser = versionHead >>= return . Version . map digitToInt

versionParser = try multiVersionParser <|>
                unitVersionParser
                    
rtemsVersionParser :: GenParser Char st Version
rtemsVersionParser = string "rtems" >> versionParser

buildDateParser :: GenParser Char st BuildDate
buildDateParser = count 8 digit >>= return . BuildDate . map digitToInt

tarHead = string "tar"
tarTail = genParser GZ <|>
          genParser XZ <|>
          genParser BZ2

tarParser :: GenParser Char st TarFormat
tarParser = dot >> tarHead >> dot >> tarTail

packageParser = do
  title <- titleParser
  _ <- dash
  version <- versionParser
  tar <- tarParser
  return $ Source title version tar

diffParser :: GenParser Char st Package
diffParser = do
  title <- titleParser
  _ <- dash
  version <- versionParser
  _ <- dash
  rtemsVersion <- rtemsVersionParser
  _ <- dash
  date <- buildDateParser
  _ <- dot
  _ <- string "diff"
  return $ Diff title version rtemsVersion date

repoConfParser :: GenParser Char st Package
repoConfParser = do
  _ <- string "rtems-"
  rtemsVersion <- versionParser
  _ <- string "-repo-conf-0."
  buildDate <- buildDateParser
  buildVersion <- unitVersionParser
  tarFormat <- tarParser
  return $ RepoConf rtemsVersion buildDate buildVersion tarFormat

versionLinkParser :: GenParser Char st Version
versionLinkParser = versionParser <* slash

strCat = foldr (++) []
readInt :: String -> Int
readInt = read

parse' p = parse p "error"
readParser p s = case parse' p s of
                    Left x -> Nothing
                    Right x -> Just x

readPackage :: String -> Maybe Package
readPackage = readParser packageParser

readDiff :: String -> Maybe Package
readDiff = readParser diffParser

readRepoConf :: String -> Maybe Package
readRepoConf = readParser repoConfParser

readVersionLink :: String -> Maybe Version
readVersionLink = readParser versionLinkParser

testR' = parse' repoConfParser repoTest

packageTest = "binutils-2.21.1.tar.bz2"
diffTest = "gcc-g++-4.5.4-rtems4.11-20120703.diff"
repoTest = "rtems-4.11-repo-conf-0.20130311.0.tar.xz"
testP = readPackage packageTest
testD = readDiff diffTest
testR = readRepoConf repoTest
