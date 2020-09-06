{-# LANGUAGE ViewPatterns #-}

module Main
  ( main
  ) where

import System.Exit (exitFailure)

import Prime

main :: IO ()
main =
  case generate (2 ^ 107 - 1) of
    Right (extractPrime -> Just n) ->
      putStrLn (show (unPrime n) <> " is a prime")
    Right _ -> putStrLn "Incomplete proof" >> exitFailure
    Left err -> putStrLn err >> exitFailure
