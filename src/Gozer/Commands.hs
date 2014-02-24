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
import           Crypto.Random        (SystemRNG)
import           Data.Aeson           (decode)
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

-- | Produces tweets as long as asked to by loading more tweets from twitter
-- Stops producing tweets if the last result is the same as the index we
-- start at
loadTweets :: String -> Maybe Integer -> Cred Permanent -> Manager -> SystemRNG
              -> Producer Tweet IO ()
loadTweets name index creds m gen = do
  (resp, gen') <- lift $ runRequest (timelineUrl name index) creds m gen
  let tweets = fromJust $ (decode (responseBody resp) :: Maybe [Tweet])
  mapM_ yield tweets
  unless (null tweets) $ do
    let lastId = tweetId $ last tweets
    case index of
      Just x -> unless (x == lastId) $ loadTweets name (Just lastId) creds m gen'
      Nothing -> loadTweets name (Just lastId) creds m gen'

tweetsNewerThan :: NominalDiffTime -> UTCTime -> Tweet -> Bool
tweetsNewerThan period now t = tweetCreatedAt t > oldTime
  where oldTime = addUTCTime period now

deleteTweets :: Cred Permanent -> Manager -> SystemRNG
                -> Consumer Tweet IO ()
deleteTweets creds m gen = do
  tweet <- await
  (_, gen') <- lift $ runRequest (destroyUrl $ tweetId tweet) creds m gen
  deleteTweets creds m gen'

-- Wraps up several components to run the complete delete pipeline
deleteOlder :: Manager -> Cred Permanent -> SystemRNG
            -> String
            -> NominalDiffTime
            -> IO ()
deleteOlder m creds gen name days = do
  newer <- tweetsNewerThan (-60*60*24*days) <$> getCurrentTime
  runEffect $ loadTweets name Nothing creds m gen
          >-> PL.dropWhile newer
          >-> deleteTweets creds m gen
