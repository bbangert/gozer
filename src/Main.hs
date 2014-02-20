{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Main where

import Control.Applicative ((<$>))
import Crypto.Random (SystemRNG, cprgCreate, createEntropyPool)
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import System.Environment (getArgs)

import Gozer (deleteOlder, parseConfigFile)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [filename] -> runDelete filename
        _          -> showError
    where showError = ioError $ userError "Usage: gozer CONFIGFILE"

runDelete :: String -> IO ()
runDelete filename = do
    parsed <- parseConfigFile filename
    case parsed of
        (Left (_, errExpl)) -> ioError $ userError errExpl
        (Right (username, dur, creds))      -> do
            m <- newManager tlsManagerSettings
            gen :: SystemRNG <- cprgCreate <$> createEntropyPool
            deleteOlder m creds gen username dur
