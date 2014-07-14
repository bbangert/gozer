{-# LANGUAGE OverloadedStrings #-}

module Main
    (
      main
    ) where

import           Control.Applicative       ((<$>))
import           Control.Lens              ((^.))
import           Control.Monad             (void)
import           Control.Monad.Error       (join, liftIO, runErrorT)
import           Control.Monad.Logger      (runNoLoggingT)
import           Data.ByteString.Char8     (pack)
import           Data.Conduit              (($$), ($=))
import qualified Data.Conduit.List         as CL
import           Data.ConfigFile           (CPError, emptyCP, get, readfile)
import           Data.Maybe                (fromJust)
import           Data.Time.Clock           (NominalDiffTime, UTCTime,
                                            addUTCTime, getCurrentTime)
import           Data.Time.Format          (readTime)
import           Data.Time.LocalTime       (zonedTimeToUTC)
import           System.Environment        (getArgs)
import           System.Locale             (defaultTimeLocale)
import           Text.Read                 (readMaybe)
import           Web.Authenticate.OAuth    (Credential (Credential), def,
                                            oauthConsumerKey,
                                            oauthConsumerSecret)
import           Web.Twitter.Conduit       (Status, User,
                                            UserParam (ScreenNameParam), call,
                                            destroyId, runTW, sourceWithMaxId,
                                            statusCreatedAt, statusId,
                                            userTimeline, userTweets, usersShow)
import           Web.Twitter.Conduit.Monad (TWInfo, setCredential, twitterOAuth)

data ConfigSettings = ConfigSettings
    { _twitterConfig :: TWInfo
    , _username      :: String
    , _oldest        :: NominalDiffTime
    , _maintainNum   :: Maybe Int
    } deriving (Show)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [filename] -> parseConfigFile filename >>= checkConfig
        _          -> ioError $ userError "Usage: gozer CONFIGFILE"
  where
    checkConfig (Left err) =
        ioError $ userError $ "Parse error in config file: " ++ snd err
    checkConfig (Right settings) = runDelete settings

runDelete :: ConfigSettings -> IO ()
runDelete (ConfigSettings twInfo username oldest maintain) = do
    olderTweets <- oldEnough (-60*60*24*oldest) <$> getCurrentTime
    runNoLoggingT . runTW twInfo $ do
        user <- call $ usersShow username'
        sourceWithMaxId (userTimeline username')
            $= CL.filter olderTweets
            $= CL.isolate (deleteCount user maintain)
            $$ CL.mapM_  $ \status ->
                void $ call $ destroyId (status ^. statusId)
  where
    username' = ScreenNameParam username

-- | Parse a INI style config file
parseConfigFile :: String
                -> IO (Either CPError ConfigSettings)
parseConfigFile filename = runErrorT $ do
    cp                <- join $ liftIO $ readfile emptyCP filename
    accessToken       <- extractPack cp "access_token"
    accessTokenSecret <- extractPack cp "access_token_secret"
    apiKey            <- extractPack cp "api_key"
    apiSecret         <- extractPack cp "api_secret"
    time              <- dget cp "duration"
    username          <- dget cp "username"

    let tokens = twitterOAuth { oauthConsumerKey = apiKey
                              , oauthConsumerSecret = apiSecret
                              }
        creds = Credential [ ("oauth_token", accessToken)
                           , ("oauth_token_secret", accessTokenSecret)
                           ]
        dur = fromIntegral (time :: Int)

    maintainNum <- either (const Nothing) readMaybe <$>
        runErrorT (dget cp "minimum_tweets")
    return $ ConfigSettings (setCredential tokens creds def) username dur maintainNum
  where
    dget cp = get cp "DEFAULT"
    extractPack cp name = pack <$> dget cp name

-- | Convert a twitter formatted UTC string into a proper UTCTime
convertTwitterTime :: String -> UTCTime
convertTwitterTime = zonedTimeToUTC . readTime defaultTimeLocale "%a %b %d %T %Z %Y"

-- | Determine if a tweet's status creation is old enough to be deleted
oldEnough :: NominalDiffTime -> UTCTime -> Status -> Bool
oldEnough period now status = oldTime > statusDateTime
  where
    statusDate = status ^. statusCreatedAt
    statusDateTime = convertTwitterTime statusDate
    oldTime = addUTCTime period now

-- | Pull out the total tweet count for a user
tweetCount :: User -> Int
tweetCount u = fromJust $ u ^. userTweets

-- | Calculate the max tweets that can be deleted
deleteCount :: User -> Maybe Int -> Int
deleteCount user Nothing  = tweetCount user
deleteCount user (Just m) = tweetCount user - m
