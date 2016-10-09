module PageCache
  ( getPage
  , insertPage
  ) where

import Data.ByteString.Lazy.Char8 (pack)
import Data.Digest.Pure.SHA
import Network.HTTP
import System.Directory

-- Get a page from the cache.  Fetch from web if page doesn't exist.
getPage :: String -> IO String
getPage url = do
  f <- doesFileExist $ file url
  if f
    then readFile $ file url
    else do
      a <- simpleHTTP (getRequest url) >>= getResponseBody
      insertPage url a
      return a

-- Insert a page into the cache.
insertPage :: String -> String -> IO ()
insertPage url page = do
  createDirectoryIfMissing False "page-cache"
  writeFile (file url) page

file :: String -> String
file url = "page-cache/" ++ hash url ++ ".html"

hash :: String -> String
hash = showDigest . sha1 . pack

