module Gozer.Config (
      parseConfigFile
    , ConfigSettings (..)
    ) where

import           Control.Applicative   ((<$>), (<*>))
import           Control.Monad.Error   (join, liftIO, runErrorT)
import           Data.ByteString.Char8 (pack)
import           Data.ConfigFile       (CPError, emptyCP, get,
                                        readfile)
import           Data.Time.Clock       (NominalDiffTime)
import           Network.OAuth         (Cred, Permanent, Token (..), clientCred,
                                        permanentCred)
import           Text.Read             (readMaybe)

data ConfigSettings = ConfigSettings { cUsername    :: String
                                     , cOldest      :: NominalDiffTime
                                     , cCredentials :: Cred Permanent
                                     , cMaintainNum :: Maybe Int
}

parseConfigFile :: String
                -> IO (Either CPError ConfigSettings)
parseConfigFile filename = runErrorT $ do
  cp <- join $ liftIO $ readfile emptyCP filename
  let ecp = extractPack cp
  accessToken <- Token <$> ecp "access_token"
                       <*> ecp "access_token_secret"
  clientToken <- Token <$> ecp "api_key"
                       <*> ecp "api_secret"
  time <- get cp "DEFAULT" "duration"
  username <- get cp "DEFAULT" "username"
  maintainNum <- either (const Nothing) readMaybe <$>
    runErrorT (get cp "DEFAULT" "minimum_tweets")
  let dur = fromIntegral (time :: Int)
      creds = permanentCred accessToken $ clientCred clientToken
  return $ ConfigSettings username dur creds maintainNum
  where
    extractPack cp name = pack <$> get cp "DEFAULT" name
