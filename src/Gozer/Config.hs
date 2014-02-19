module Gozer.Config (
    parseConfigFile
    ) where

import Data.ByteString.Char8 (pack)
import Control.Applicative ((<$>), (<*>))
import Control.Monad.Error (runErrorT, liftIO, join)
import Data.ConfigFile (readfile, emptyCP, get, CPError)
import Network.OAuth (
    clientCred, permanentCred,
    Cred, Token(..), Permanent
    )

parseConfigFile :: String -> IO (Either CPError (Cred Permanent))
parseConfigFile filename = do
    rv <- runErrorT $ do
        cp <- join $ liftIO $ readfile emptyCP filename
        accessToken <- Token
            <$> (pack <$> get cp "DEFAULT" "access_token")
            <*> (pack <$> get cp "DEFAULT" "access_token_secret")
        clientToken <- Token
            <$> (pack <$> get cp "DEFAULT" "api_key")
            <*> (pack <$> get cp "DEFAULT" "api_secret")
        return $ permanentCred accessToken $ clientCred clientToken
    return rv
