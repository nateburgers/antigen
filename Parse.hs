module Parse where
import Data
import Text.ParserCombinators.Parsec
import Control.Applicative hiding ((<|>))
import Data.Char

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
  _ <- dot
  buildVersion <- digit
  tarFormat <- tarParser
  return $ RepoConf rtemsVersion buildDate (Version [digitToInt buildVersion]) tarFormat

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

packageTest = "binutils-2.21.1.tar.bz2"
diffTest = "gcc-g++-4.5.4-rtems4.11-20120703.diff"
repoTest = "rtems-4.11-repo-conf-0.20130311.0.tar.xz"
testP = readPackage packageTest
testD = readDiff diffTest
testR = readRepoConf repoTest
