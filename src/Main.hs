-- | Haskell version of a <http://hermanradtke.com/2015/09/21/get-data-from-a-url-rust.html Rust program>.

{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.Wreq (get, responseStatus, responseBody, statusCode)
import Data.Monoid ((<>))
import Control.Monad (when)
import Control.Lens ((^.), (^?), itraverseOf_)
import Data.Aeson.Lens (key, members, _String, AsValue)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO

url :: String
url = "https://www.hautelook.com/api"

main :: IO ()
main = do
  r <- get url
  let code = r ^. responseStatus . statusCode
  when (code /= 200) $
    fail ("Unable to handle HTTP response code " ++ show code)
  case r ^? responseBody . key "_links" of
    Nothing -> fail "Failed to get '_links' value from json"
    Just links -> itraverseOf_ members printLink links

printLink :: AsValue s => Text.Text -> s -> IO ()
printLink rel link =
  case link ^? key "href" . _String of
    Nothing -> fail $ Text.unpack $
      "Failed to get 'href' value from '_links' at key " <> rel
    Just href -> TextIO.putStrLn $ rel <> " -> " <> href
