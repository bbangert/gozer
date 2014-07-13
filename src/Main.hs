{-# LANGUAGE OverloadedStrings #-}

module Main
    (
      main
    ) where

import           Control.Applicative  ((<$>))
import           Control.Lens         ((^.))
import           Control.Monad        (void)
import           Control.Monad.Logger (runNoLoggingT)
import           Data.Conduit         (($$), ($=))
import qualified Data.Conduit.List    as CL
import           Data.Maybe           (fromJust)
import           Data.Time.Clock      (NominalDiffTime, UTCTime, addUTCTime,
                                       getCurrentTime)
import           Data.Time.Format     (readTime)
import           Data.Time.LocalTime  (zonedTimeToUTC)
import           System.Environment   (getArgs)
import           System.Locale        (defaultTimeLocale)
import           Web.Twitter.Conduit  (Status, User,
                                       UserParam (ScreenNameParam), call,
                                       destroyId, runTW, sourceWithMaxId,
                                       statusCreatedAt, statusId, userTimeline,
                                       userTweets, usersShow)

import           Gozer                (ConfigSettings (..), parseConfigFile)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filename] -> runDelete filename
    _          -> ioError $ userError "Usage: gozer CONFIGFILE"

runDelete :: String -> IO ()
runDelete filename = do
  parsed <- parseConfigFile filename
  case parsed of
    Left (_, errExpl) -> ioError $ userError
                                 $ "Parse error in config file: " ++ errExpl
    Right (ConfigSettings twInfo username oldest maintain) -> do
        let username' = ScreenNameParam username
        olderTweets <- oldEnough (-60*60*24*oldest) <$> getCurrentTime
        runNoLoggingT . runTW twInfo $ do
            user <- call $ usersShow username'
            sourceWithMaxId (userTimeline username')
                $= CL.filter olderTweets
                $= CL.isolate (deleteCount user maintain)
                $$ CL.mapM_  $ \status ->
                    void $ call $ destroyId (status ^. statusId)
        return ()

convertTwitterTime :: String -> UTCTime
convertTwitterTime = zonedTimeToUTC . readTime defaultTimeLocale "%a %b %d %T %Z %Y"

oldEnough :: NominalDiffTime -> UTCTime -> Status -> Bool
oldEnough period now status = oldTime > statusDateTime
  where
    statusDate = status ^. statusCreatedAt
    statusDateTime = convertTwitterTime statusDate
    oldTime = addUTCTime period now

tweetCount :: User -> Int
tweetCount u = fromJust $ u ^. userTweets

deleteCount :: User -> Maybe Int -> Int
deleteCount user Nothing  = tweetCount user
deleteCount user (Just m) = tweetCount user - m
