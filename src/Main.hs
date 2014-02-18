{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Main where

import Control.Applicative ((<$>))
import Crypto.Random (SystemRNG, cprgCreate, createEntropyPool)
import Data.Time.Clock (getCurrentTime)
import Network.OAuth (
    clientCred, permanentCred,
    Client, Cred, Token(..), Permanent
    )
import Network.HTTP.Client (
    newManager,
    Manager
    )
import Network.HTTP.Client.TLS (tlsManagerSettings)

accessToken :: Token Permanent
accessToken = Token "TOK" "TOKSECRET"

clientCreds :: Cred Client
clientCreds = clientCred $ Token "TOK" "TOKSECRET"

loadCredentials :: Cred Permanent
loadCredentials = permanentCred accessToken clientCreds

main :: IO (Cred Permanent, Manager, SystemRNG)
main = do
    m <- newManager tlsManagerSettings
    gen :: SystemRNG <- cprgCreate <$> createEntropyPool
    let creds = loadCredentials
    return (creds, m, gen)
