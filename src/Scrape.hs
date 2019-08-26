{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Scrape where

import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as HTTP
import qualified Network.HTTP.Types.Header as HTTP
import qualified Data.ByteString.Lazy as B
import qualified Data.Text    as T
import qualified Data.Text.IO as T
import qualified Text.Parsec.Token as Tok

--import           Text.Parsec.Language (haskellStyle)
--import           Text.Parsec.String
--import           Text.Parsec.Char
--import           Text.Parsec.Char
--import           Text.Parsec.String
--import           Text.ParserCombinators.Parsec hiding (State)
--import           Text.ParserCombinators.Parsec.Char

import           Network.HTTP.Conduit
import           Web.Authenticate.OAuth
import           Data.Text hiding (intersperse)
import           Data.List (isInfixOf, intersperse)
import           Text.Regex.TDFA
import           System.IO
import           Control.Monad
import           Text.HTML.Scalpel
import           Control.Applicative hiding ((<|>), many)
import           Data.Default (def)
import           System.Environment


-- Create a new manager settings based on the default TLS manager that updates
-- the request headers to include a custom user agent.
managerSettings :: HTTP.ManagerSettings
managerSettings = HTTP.tlsManagerSettings {
  HTTP.managerModifyRequest = \req -> do
    req' <- HTTP.managerModifyRequest HTTP.tlsManagerSettings req
    return $ req' {
      HTTP.requestHeaders = (HTTP.hUserAgent, "Mozilla/5.0 (Windows NT 6.1; rv:60.0) Gecko/20100101 Firefox/60.0")
                          : HTTP.requestHeaders req'
    }
  }

--tags :: [Tag String]
--tags = [TagText "text", TagComment "comment"]

data Coin = String URL
--
{-
brackets :: Parser a -> Parser a
brackets m =
  char '<' >>= \_ ->
    m >>= \n ->
      char '>' >>= \_ ->
        return n

quotes :: Parser a -> Parser a
quotes m =
  ( try $ char '\"' <|> char '\'' ) >>= \_ ->
    m >>= \n ->
      ( try $ char '\"' <|> char '\'' ) >>= \_ ->
        return n

parseURL :: Parser [(String, String)]
parseURL =
  ( brackets >>= \_ -> 
    string "href=" >>= \_ -> 
      stringLit ) >>= \x ->
        word >>= \y ->
          brackets many1 >>= \_ ->
            return [(y, x)]

stringLit :: Parser String
stringLit =
  quotes $ many (noneOf "\"") >>= \x ->
    return x

word :: Parser String
word = do
  x <- many (noneOf "\"")
  char '<'
  return x
-}

scraper :: IO ()
scraper = do
  --url >>= handleArgs
  --urlToMd url
  manager <- Just <$> HTTP.newManager managerSettings
  x       <- def manager
  html    <- scrapeURLWithConfig x url $ htmls coins
  let urls = process html 
  maybe printError printHtml html
    where
      url = "https://coinmarketcap.com/all/views/all/"
      printError = putStrLn "Failed"
      printHtml = mapM_ putStrLn
      --process :: Maybe [String] -> [String] -> [String]
      --process Nothing   _       = []
      --process (Just (x:[])) acc = acc
      --process (Just (x:xs)) acc = process (Just xs) 
      --  (acc ++ [x =~~ "[a-zA-Z0-9-]" 
      --    :: String])
      --coins :: Scraper String [Coin]
      -- parseURLS :: Parser
      coins = "a" @: [hasClass "currency-name-container"] -- $ do
        --contents <- text anySelector
        --guard ("href" contents)
        --html anySelector
