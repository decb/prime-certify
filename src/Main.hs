{-# LANGUAGE ViewPatterns #-}

module Main
  ( main
  ) where

import System.Exit (exitFailure)

import Examples
import Prime

main :: IO ()
main =
  case prime3 of
    Right (extractPrime -> Just (unPrime -> 3)) -> putStrLn "3 is a prime"
    Right _ -> putStrLn "Unexpected proof" >> exitFailure
    Left err -> putStrLn err >> exitFailure
