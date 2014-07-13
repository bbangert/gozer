{-# LANGUAGE OverloadedStrings #-}

module Gozer.Config
    (
      ConfigSettings (..)
    , parseConfigFile

    ) where

import           Control.Applicative       ((<$>))
import           Control.Monad.Error       (join, liftIO, runErrorT)
import           Data.ByteString.Char8     (pack)
import           Data.ConfigFile           (CPError, emptyCP, get, readfile)
import           Data.Time.Clock           (NominalDiffTime)
import           Text.Read                 (readMaybe)
import           Web.Authenticate.OAuth    (Credential (Credential), def,
                                            oauthConsumerKey,
                                            oauthConsumerSecret)
import           Web.Twitter.Conduit.Monad (TWInfo, setCredential, twitterOAuth)

data ConfigSettings = ConfigSettings
    { csTwitterConfig :: TWInfo
    , csUsername      :: String
    , csOldest        :: NominalDiffTime
    , csMaintainNum   :: Maybe Int
    } deriving (Show)

-- | Parse a INI style config file
parseConfigFile :: String
                -> IO (Either CPError ConfigSettings)
parseConfigFile filename = runErrorT $ do
    cp <- join $ liftIO $ readfile emptyCP filename
    let ecp = extractPack cp

    accessToken       <- ecp "access_token"
    accessTokenSecret <- ecp "access_token_secret"
    let creds = Credential [ ("oauth_token", accessToken)
                           , ("oauth_token_secret", accessTokenSecret)
                           ]

    apiKey    <- ecp "api_key"
    apiSecret <- ecp "api_secret"
    let tokens = twitterOAuth { oauthConsumerKey = apiKey
                              , oauthConsumerSecret = apiSecret
                              }

    time        <- dget cp "duration"
    username    <- dget cp "username"
    maintainNum <- either (const Nothing) readMaybe <$>
        runErrorT (dget cp "minimum_tweets")
    let dur = fromIntegral (time :: Int)
    return $ ConfigSettings (setCredential tokens creds def) username dur maintainNum
  where
    dget cp = get cp "DEFAULT"
    extractPack cp name = pack <$> dget cp name
