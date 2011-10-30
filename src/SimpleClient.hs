{-# LANGUAGE OverloadedStrings #-}
module Main where

import Network.HTTP.Enumerator
    ( http, parseUrl, withManager, RequestBody (RequestBodyLBS)
    , requestBody
    )
import Data.Aeson (Value (Object, String))
import qualified Data.Map as Map
import Data.Aeson.Parser (json)
import Data.Attoparsec.Enumerator (iterParser)
import Control.Monad.IO.Class (liftIO)
import Data.Enumerator (run_)
import Data.Aeson.Encode (fromValue)
import Blaze.ByteString.Builder (toLazyByteString)

main :: IO ()
main = withManager $ \manager -> do
    value <- makeValue
    -- We need to know the size of the request body, so we convert to a
    -- ByteString
    let valueBS = toLazyByteString $ fromValue value
    req' <- parseUrl "http://localhost:3000/"
    let req = req' { requestBody = RequestBodyLBS valueBS }
    run_ $ flip (http req) manager $ \status headers -> do
        -- Might want to ensure we have a 200 status code and Content-Type is
        -- application/json. We skip that here.
        resValue <- iterParser json
        liftIO $ handleResponse resValue

-- Application-specific function to make the request value
makeValue :: IO Value
makeValue = return $ Object $ Map.fromList
    [ ("foo", String "bar")
    ]

-- Application-specific function to handle the response from the server
handleResponse :: Value -> IO ()
handleResponse = print
