module Main where
import Parse
import Text.XML.HXT.Core
import Data.Maybe
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Maybe
import Network.HTTP
import Network.URI


rtemsRoot = "http://www.rtems.com/ftp/pub/rtems/SOURCES/"

rtemsPage :: String -> String
rtemsPage s = rtemsRoot ++ s ++ "/"

-- this produces wrong urls
rtemsRoute :: [String] -> String
rtemsRoute = foldl addDir rtemsRoot
    where addDir x xs = x ++ "/" ++ xs

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

--main :: IO [Package] -- old main
main = do
  links <- scrape rtemsRoot
  packageLinks <- mapM (scrape . rtemsPage . show) $ versionLinksFor links
  return $ map packagesFor packageLinks
      where versionLinksFor xs = compact $ map readVersionLink xs
            packagesFor links = compact $ map readPackage links

