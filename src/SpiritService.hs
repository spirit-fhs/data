{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.Wai (Response (ResponseBuilder), Application)
import Network.HTTP.Types (status200, status400)
import Network.Wai.Handler.Warp (run)
import Data.Aeson.Parser (json)
import Data.Attoparsec.Enumerator (iterParser)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Value (Object, String))
import Data.Aeson.Encode (fromValue)
import Data.Enumerator (catchError, Iteratee)
import Control.Exception (SomeException)
import Data.ByteString (ByteString)
import qualified Data.Map as Map
import Data.Text (pack)

main :: IO ()
main = run 3000 app

app :: Application
app _ = flip catchError invalidJson $ do
    value <- iterParser json
    newValue <- liftIO $ modValue value
    return $ ResponseBuilder
        status200
        [("Content-Type", "application/json")]
        $ fromValue newValue

invalidJson :: SomeException -> Iteratee ByteString IO Response
invalidJson ex = return $ ResponseBuilder
    status400
    [("Content-Type", "application/json")]
    $ fromValue $ Object $ Map.fromList
        [ ("message", String $ pack $ show ex)
        ]

-- Application-specific logic would go here.
modValue :: Value -> IO Value
modValue = return
