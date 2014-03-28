{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Gozer.Commands (
    loadTweets,
    tweetsNewerThan,
    deleteTweets,
    deleteOlder,
    ) where

import           Control.Applicative  ((<$>))
import           Control.Monad        (unless)
import           Crypto.Random        (SystemRNG, cprgFork)
import           Data.Aeson           (decode, (.:))
import           Data.Aeson.Types     (parseMaybe)
import           Data.ByteString.Lazy (ByteString)
import           Data.Maybe           (fromJust)
import           Data.Time.Clock      (NominalDiffTime, UTCTime, addUTCTime,
                                       getCurrentTime)
import           Network.HTTP.Client  (Manager, Request, Response, httpLbs,
                                       method, parseUrl, responseBody)
import           Network.OAuth        (Cred, Permanent, defaultServer, oauth)
import           Pipes                (Consumer, Producer, await, lift,
                                       runEffect, yield, (>->))
import qualified Pipes.Prelude        as PL

import           Gozer.Config         (ConfigSettings (ConfigSettings))
import           Gozer.Types          (Tweet, tweetCreatedAt, tweetId)

-- | Signs and runs a request to get back a response
runRequest :: Request -> Cred Permanent -> Manager -> SystemRNG
              -> IO (Response ByteString,  SystemRNG)
runRequest req creds m gen = do
  (signedReq, gen') <- oauth creds defaultServer req gen
  resp <- httpLbs signedReq m
  return (resp, gen')

-- | Timeline URL for retrieving tweets
timelineUrl :: String -> Maybe Integer -> Request
timelineUrl name sinceId = fromJust . parseUrl $ concat
  [ "https://api.twitter.com/1.1/statuses/user_timeline.json?screen_name="
  , name
  , "&include_rts=true&count=200"
  , maybe "" (("&max_id=" ++) . show) sinceId
  ]

-- | Destroy URL for deleting tweets
destroyUrl :: Integer -> Request
destroyUrl i = (makeUrl i) {method="POST"}
  where makeUrl = fromJust . parseUrl
          . ("https://api.twitter.com/1.1/statuses/destroy/"++)
          . (++".json") . show

-- | Tweet information for the user
informationURL :: String -> Request
informationURL name = fromJust . parseUrl $
  "https://api.twitter.com/1.1/users/show.json?screen_name=" ++ name

-- | Decode the tweet count from a JSON response
decodeTweetCount :: ByteString -> Int
decodeTweetCount = fromJust . parseMaybe (.: "statuses_count") . fromJust . decode

-- | Return the difference between the users current tweets and the given
-- vale. If Nothing, the full count is returned, 0 is returned if its negative.
tweetDiff :: String -> Maybe Int -> Cred Permanent -> Manager -> SystemRNG -> IO Int
tweetDiff name tweetMin creds m gen = do
  resp <- fst <$> runRequest (informationURL name) creds m gen
  let tweetCount = decodeTweetCount $ responseBody resp
  return $ maybe tweetCount (\i -> max (tweetCount-i) 0) tweetMin

-- | Produces tweets as long as asked to by loading more tweets from twitter
-- Stops producing tweets if the last result is the same as the index we
-- start at
loadTweets :: String -> Maybe Integer -> Cred Permanent -> Manager -> SystemRNG
              -> Producer Tweet IO ()
loadTweets name index creds m gen = do
  (resp, gen') <- lift $ runRequest (timelineUrl name index) creds m gen
  let tweets = fromJust (decode (responseBody resp) :: Maybe [Tweet])
  mapM_ yield tweets
  unless (null tweets) $ do
    let lastId = tweetId $ last tweets
    case index of
      Just x -> unless (x == lastId) $ loadTweets name (Just lastId) creds m gen'
      Nothing -> loadTweets name (Just lastId) creds m gen'

-- | Indicates if the tweet is newer than the time diff + a time
tweetsNewerThan :: NominalDiffTime -> UTCTime -> Tweet -> Bool
tweetsNewerThan period now t = tweetCreatedAt t > oldTime
  where oldTime = addUTCTime period now

-- | Issues a delete API call for every tweet received
deleteTweets :: Cred Permanent -> Manager -> SystemRNG
                -> Consumer Tweet IO ()
deleteTweets creds m gen = do
  tweet <- await
  (_, gen') <- lift $ runRequest (destroyUrl $ tweetId tweet) creds m gen
  deleteTweets creds m gen'

-- Wraps up several components to run the complete delete pipeline
deleteOlder :: Manager -> SystemRNG -> ConfigSettings -> IO ()
deleteOlder m gen (ConfigSettings name days creds tweetMin) = do
  let (diffGen, gen') = cprgFork gen
      (loadGen, deleteGen) = cprgFork gen'
  tweetNum <- tweetDiff name tweetMin creds m diffGen
  newer <- tweetsNewerThan (-60*60*24*days) <$> getCurrentTime
  runEffect $ loadTweets name Nothing creds m loadGen
          >-> PL.dropWhile newer
          >-> PL.take tweetNum
          >-> deleteTweets creds m deleteGen
