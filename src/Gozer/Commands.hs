{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Gozer.Commands (
    loadTweets,
    tweetListWhile,
    tweetsNewerThan,
    deleteTweets,
    ) where

import Crypto.Random (SystemRNG)
import Data.Aeson (decode)
import Data.ByteString.Lazy (ByteString)
import Data.Maybe (fromJust)
import Data.Time.Clock (UTCTime,  NominalDiffTime, addUTCTime)
import Network.OAuth (
    oauth, defaultServer,
    Cred, Permanent
    )
import Network.HTTP.Client (
    httpLbs, responseBody, method,
    Request, Response, Manager
    )
import Network.HTTP.Client (parseUrl)
import Pipes (
    lift, yield, await,
    Producer, Consumer
    )
import qualified Pipes as P
import qualified Pipes.Prelude as PL

import Gozer.Types (Tweet, tweetId, tweetCreatedAt)

-- | Signs and runs a request to get back a response
runRequest :: Request -> Cred Permanent -> Manager -> SystemRNG
              -> IO (Response ByteString,  SystemRNG)
runRequest req creds m gen = do
    (signedReq, gen') <- oauth creds defaultServer req gen
    resp <- httpLbs signedReq m
    return $ (resp, gen')

-- | Timeline URL for retrieving tweets
timelineUrl :: String -> Maybe Integer -> Request
timelineUrl name sinceId = fromJust . parseUrl $ concat
    [ "https://api.twitter.com/1.1/statuses/user_timeline.json?screen_name="
    , name
    , "&include_rts=false&count=200"
    , maybe "" (("&max_id=" ++) . show) sinceId
    ]

-- | Destroy URL for deleting tweets
destroyUrl :: Integer -> Request
destroyUrl i = (makeUrl i) {method="POST"}
    where makeUrl = fromJust . parseUrl
            . ("https://api.twitter.com/1.1/statuses/destroy/"++)
            . (++".json") . show

-- | Produces tweets as long as asked to by loading more tweets from twitter
loadTweets :: String -> Maybe Integer -> Cred Permanent -> Manager -> SystemRNG
              -> Producer Tweet IO ()
loadTweets name index creds m gen = do
    (resp, gen') <- lift $ runRequest (timelineUrl name index) creds m gen
    let tweets = fromJust $ (decode (responseBody resp) :: Maybe [Tweet])
    mapM_ yield tweets
    let lastId = tweetId $ last tweets
    loadTweets name (Just lastId) creds m gen'

tweetListWhile :: String -> Cred Permanent -> Manager -> SystemRNG
                  -> (Tweet -> Bool)
                  -> IO [Tweet]
tweetListWhile name creds m gen predicate = PL.toListM $
    loadTweets name Nothing creds m gen P.>-> PL.takeWhile predicate

tweetsNewerThan :: NominalDiffTime -> UTCTime -> Tweet -> Bool
tweetsNewerThan period now t = tweetCreatedAt t > oldTime
    where oldTime = addUTCTime period now

deleteTweets :: Cred Permanent -> Manager -> SystemRNG
                -> Consumer Tweet IO ()
deleteTweets creds m gen = do
    tweet <- await
    (_, gen') <- lift $ runRequest (destroyUrl $ tweetId tweet) creds m gen
    deleteTweets creds m gen'
