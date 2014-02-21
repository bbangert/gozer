{-# LANGUAGE OverloadedStrings #-}
module Gozer.Types (
    Tweet,
    tweetCreatedAt, tweetId, tweetText,
    ) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (mzero)
import Data.Aeson (parseJSON, FromJSON, Value(Object), (.:), Object)
import Data.Aeson.Types (Parser)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.Time.LocalTime (zonedTimeToUTC)

data Tweet = Tweet
    { tweetCreatedAt  :: UTCTime
    , tweetText       :: Text
    , tweetId         :: Integer
    } deriving (Show)

-- Parser that handles ZonedTime but converts them to UTCTime
zonedToUTC :: Object -> Text -> Parser UTCTime
zonedToUTC obj name = zonedTimeToUTC <$> obj .: name

-- Parse out the portions of the tweet we care about, the rest of the
-- attributes are ignored
instance FromJSON Tweet where
    parseJSON (Object v) = Tweet <$> v `zonedToUTC` "created_at"
                                 <*> v .: "text"
                                 <*> v .: "id"
    parseJSON _          = mzero
