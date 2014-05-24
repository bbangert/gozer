{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Gozer.Commands (
    deleteOlder,
    ) where

import           Control.Applicative        ((<$>))
import           Control.Monad              (unless, void)
import           Control.Monad.Reader       (ReaderT, runReaderT, ask)
import           Control.Monad.State.Strict (StateT, runStateT, get, put)
import           Control.Monad.Trans        (liftIO)
import           Crypto.Random              (SystemRNG, cprgFork)
import           Data.Aeson                 (decode, (.:))
import           Data.Aeson.Types           (parseMaybe)
import           Data.ByteString.Lazy       (ByteString)
import           Data.Maybe                 (fromJust)
import           Data.Time.Clock            (NominalDiffTime, UTCTime,
                                             addUTCTime, getCurrentTime)
import           Network.HTTP.Client        (Manager, Request, Response,
                                             httpLbs, method, parseUrl,
                                             responseBody)
import           Network.OAuth              (Cred, Permanent, defaultServer,
                                             oauth)
import           Pipes                      (Consumer, Producer, await, lift,
                                             runEffect, yield, (>->))
import qualified Pipes.Prelude              as PL

import           Gozer.Config               (ConfigSettings (ConfigSettings))
import           Gozer.Types                (Tweet, tweetCreatedAt, tweetId)

data TwitterConfig = TwitterConfig (Cred Permanent) Manager

type TwitterCommand = ReaderT TwitterConfig (StateT SystemRNG IO)

fromUrl :: String -> Request
fromUrl = fromJust . parseUrl

-- | Decode the tweet count from a JSON response
decodeTweetCount :: ByteString -> Int
decodeTweetCount = fromJust . parseMaybe (.: "statuses_count") . fromJust . decode

runTwitterCommand :: TwitterCommand a -> TwitterConfig -> SystemRNG
                  -> IO (a, SystemRNG)
runTwitterCommand tc conf = runStateT (runReaderT tc conf)

-- | Signs and runs a request to get back a response
runRequest :: Request -> TwitterCommand (Response ByteString)
runRequest req = do
  TwitterConfig creds m <- ask
  gen <- get
  (signedReq, gen') <- liftIO $ oauth creds defaultServer req gen
  put gen'
  liftIO $ httpLbs signedReq m

-- | Timeline URL for retrieving tweets
timelineUrl :: String -> Maybe Integer -> Request
timelineUrl name sinceId = fromUrl $ concat
  [ "https://api.twitter.com/1.1/statuses/user_timeline.json?screen_name="
  , name
  , "&include_rts=true&count=200"
  , maybe "" (("&max_id=" ++) . show) sinceId
  ]

-- | Destroy URL for deleting tweets
destroyUrl :: Integer -> Request
destroyUrl i = (makeUrl i) {method="POST"}
  where makeUrl = fromUrl
          . ("https://api.twitter.com/1.1/statuses/destroy/"++)
          . (++".json") . show

-- | Tweet information for the user
informationURL :: String -> Request
informationURL name = fromUrl $
  "https://api.twitter.com/1.1/users/show.json?screen_name=" ++ name

-- | Return the difference between the users current tweets and the given
-- vale. If Nothing, the full count is returned, 0 is returned if its negative.
tweetDiff :: String -> Maybe Int -> TwitterCommand Int
tweetDiff name tweetMin = do
  resp <- runRequest (informationURL name)
  let tweetCount = decodeTweetCount $ responseBody resp
  return $ maybe tweetCount (\i -> max (tweetCount-i) 0) tweetMin

-- | Loads tweets starting at the offset asked
loadTweets :: String -> Maybe Integer -> TwitterCommand [Tweet]
loadTweets name index = do
  resp <- runRequest $ timelineUrl name index
  return $ fromJust (decode (responseBody resp) :: Maybe [Tweet])

-- | Delete a tweet
deleteTweet :: Tweet -> TwitterCommand ()
deleteTweet = void . runRequest . destroyUrl . tweetId

-- | Produces tweets as long as asked to by loading more tweets from twitter
-- Stops producing tweets if the last result is the same as the index we
-- start at
tweetProducer :: String -> Maybe Integer -> TwitterConfig -> SystemRNG
              -> Producer Tweet IO ()
tweetProducer name index tc gen = do
  (tweets, gen') <- lift $ runTwitterCommand (loadTweets name index) tc gen
  mapM_ yield tweets
  unless (null tweets) $ do
    let lastId = tweetId $ last tweets
    case index of
      Just x -> unless (x == lastId) $ tweetProducer name (Just lastId) tc gen'
      Nothing -> tweetProducer name (Just lastId) tc gen'

-- | Indicates if the tweet is newer than the time diff + a time
tweetsNewerThan :: NominalDiffTime -> UTCTime -> Tweet -> Bool
tweetsNewerThan period now t = tweetCreatedAt t > oldTime
  where oldTime = addUTCTime period now

-- | Issues a delete API call for every tweet received
tweetDeleter :: TwitterConfig -> SystemRNG -> Consumer Tweet IO ()
tweetDeleter tc gen = do
  tweet <- await
  (_, gen') <- lift $ runTwitterCommand (deleteTweet tweet) tc gen
  tweetDeleter tc gen'

-- Wraps up several components to run the complete delete pipeline
deleteOlder :: Manager -> SystemRNG -> ConfigSettings -> IO ()
deleteOlder m gen (ConfigSettings name days creds tweetMin) = do
  let (diffGen, gen') = cprgFork gen
      (loadGen, deleteGen) = cprgFork gen'
      tc = TwitterConfig creds m
  (tweetNum, _) <- runTwitterCommand (tweetDiff name tweetMin) tc diffGen
  newer <- tweetsNewerThan (-60*60*24*days) <$> getCurrentTime
  runEffect $ tweetProducer name Nothing tc loadGen
          >-> PL.dropWhile newer
          >-> PL.take tweetNum
          >-> tweetDeleter tc deleteGen
