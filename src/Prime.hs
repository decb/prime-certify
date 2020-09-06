module Prime
  ( Prime
  , Proof
  , Prove
  , axiom
  , back
  , extractPrime
  , generate
  , rule1
  , rule2
  , unPrime
  ) where

import Control.Monad (foldM)
import Control.Monad.Except (throwError)
import Control.Monad.State.Strict (StateT, evalStateT, get, modify)
import Data.List (find)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

newtype Prime =
  Prime Integer
  deriving (Show)

data Derivation a
  = Axiom Integer
          Integer
  | Rule1 a
          a
  | Rule2 a
  deriving (Show)

data ProofValue
  = Triple Integer
           Integer
           Integer
  | Single Integer
  deriving (Show)

data Proof =
  Proof ProofValue
        (Derivation Proof)
  deriving (Show)

type Prove = Either String

back :: Proof -> Derivation Proof
back (Proof _ d) = d

extractPrime :: Proof -> Maybe Prime
extractPrime (Proof (Single p) _) = return $ Prime p
extractPrime _ = Nothing

unPrime :: Prime -> Integer
unPrime (Prime n) = n

axiom :: Integer -> Integer -> Prove Proof
axiom x y
  | x >= 1 && y >= 1 = return $ Proof (Triple x y 1) (Axiom x y)
  | otherwise = throwError "Can't apply axiom"

rule1 :: Proof -> Proof -> Prove Proof
rule1 b1@(Proof (Triple p x a) _) b2@(Proof (Single q) _)
  | modExp x ((p - 1) `div` q) p /= 1 && q `divides` (p - 1) =
    return $ Proof (Triple p x (q * a)) (Rule1 b1 b2)
rule1 _ _ = throwError "Can't apply rule1"

rule2 :: Proof -> Prove Proof
rule2 b@(Proof (Triple p x p') _)
  | p' == p - 1 && modExp x p' p == 1 = return $ Proof (Single p) (Rule2 b)
rule2 _ = throwError "Can't apply rule2"

modExp :: Integer -> Integer -> Integer -> Integer
modExp x y m = go x y 1
  where
    go x 0 r = r
    go x y r =
      let x' = mod (x * x) m
      in case y `quotRem` 2 of
           (y', 1) -> go x' y' (r * x `mod` m)
           (y', _) -> go x' y' r

divides :: Integer -> Integer -> Bool
divides n m = m `mod` n == 0

generate :: Integer -> Prove Proof
generate n =
  case axiom 2 1 >>= rule2 of
    Right p2 -> evalStateT (generate' n) (Map.singleton 2 p2)
    Left err -> throwError err

generate' :: Integer -> StateT (Map Integer Proof) Prove Proof
generate' n = do
  map <- get
  case Map.lookup n map of
    Just proof -> return proof
    Nothing ->
      let ps = primeFactors (n - 1)
          witness =
            find
              (\m ->
                 modExp m (n - 1) n == 1 &&
                 all (\p -> modExp m ((n - 1) `div` p) n /= 1) ps)
              [2 .. n - 1]
      in case witness of
           Just w -> do
             sub <- mapM generate' ps
             let result = do
                   a <- axiom n w
                   r <- foldM rule1 a sub
                   rule2 r
             case result of
               Right proof -> do
                 modify (Map.insert n proof)
                 return proof
               Left err -> throwError err
           Nothing -> throwError ("Can't find witness for " <> show n)

primeFactors :: Integer -> [Integer]
primeFactors = go 2
  where
    go d n
      | d * d > n = [n]
      | otherwise =
        case n `divMod` d of
          (n', 0) -> d : go d n'
          _ -> go (d + 1) n
