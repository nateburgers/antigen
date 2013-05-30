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
            
openURL :: String -> MaybeT IO String
openURL url = case parseURI url of
                Nothing -> fail "could not read URI"
                Just u -> liftIO (getResponseBody =<< simpleHTTP (mkRequest GET u))

get url = do
  contents <- runMaybeT $ openURL url
  return $ readString [withParseHTML yes, withWarnings no] (fromMaybe "" contents)

scrape url = runX . xshow =<< get url
--scrape' url query = runX . xshow =<< (get url) >>> query
scrape' url = do
  page <- get url
  return $ runX $ page /> getText
