module Data where
import Text.ParserCombinators.Parsec

data Title = Title String
data Version = Version [Int]
data BuildDate = BuildDate [Int]
data TarFormat = BZ2 | GZ | XZ
data Package = Source Title Version TarFormat
             | Diff Title Version Version BuildDate
             | RepoConf Version BuildDate Version TarFormat

packageFolderName :: Package -> String
packageFolderName (Source t v tar) = foldCat [show t, "-", show v]
packageFolderName (RepoConf v b bv t) = foldCat ["rtems-", show v, "-repo-conf-0.", show b, ".", show bv]
packageFolderName _ = ""

packageVersion :: Package -> Version
packageVersion (Source _ v _) = v
packageVersion (Diff _ v _ _) = v
packageVersion (RepoConf _ _ bv t) = bv

packageVersionMatches :: Package -> Version -> Bool
packageVersionMatches p v = packageVersion p == v

data ConfigureOption = String
data MetaPackage = MetaPackage
                 { package :: Package
                 , patch :: Maybe Package
                 , configureOptions :: [ConfigureOption]
                 }

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
    show (RepoConf v b bv t) = foldCat ["rtems-", show v, "-repo-conf-0.", show b, ".", show bv, ".", show t]

instance Eq Version where
    (==) (Version a) (Version b) = a == b

instance Eq BuildDate where
    (==) (BuildDate a) (BuildDate b) = a == b

instance Eq Package where
    (==) (Source (Title ta) _ _) (Source (Title tb) _ _) = ta == tb
    (==) (Diff (Title ta) _ _ _) (Diff (Title tb) _ _ _) = ta == tb
    (==) (RepoConf _ (BuildDate ba) _ _) (RepoConf _ (BuildDate bb) _ _) = ba == bb
                                                   
instance Ord Package where
    (<=) (Source _ (Version va) _) (Source _ (Version vb) _) = va <= vb
    (<=) (Diff _ _ _ (BuildDate ba)) (Diff _ _ _ (BuildDate bb)) = ba <= bb
    (<=) (RepoConf _ (BuildDate ba) (Version va) _) (RepoConf _ (BuildDate bb) (Version vb) _) =
        if ba == bb
        then va <= vb
        else ba <= bb

foldCat :: [String] -> String
foldCat = foldr (++) ""
