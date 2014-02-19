{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Main where

import Control.Applicative ((<$>))
import Crypto.Random (SystemRNG, cprgCreate, createEntropyPool)
import Data.Time.Clock (getCurrentTime)
import Network.OAuth (Cred, Permanent)
import Network.HTTP.Client (Manager, newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)

main :: IO (Cred Permanent, Manager, SystemRNG)
main = do
    m <- newManager tlsManagerSettings
    gen :: SystemRNG <- cprgCreate <$> createEntropyPool
    let creds = loadCredentials
    return (creds, m, gen)
