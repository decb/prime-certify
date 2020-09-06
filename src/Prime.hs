module Prime
  ( Prime
  , Proof
  , Prove
  , axiom
  , extractPrime
  , rule1
  , rule2
  , unPrime
  ) where

newtype Prime =
  Prime Integer
  deriving (Show)

data Proof
  = Triple (Integer, Integer, Integer)
  | Single Integer
  deriving (Show)

type Prove a = Either String a

extractPrime :: Proof -> Maybe Prime
extractPrime (Single p) = return $ Prime p
extractPrime _ = Nothing

unPrime :: Prime -> Integer
unPrime (Prime n) = n

axiom :: Integer -> Integer -> Prove Proof
axiom x y
  | x >= 1 && y >= 1 = return $ Triple (x, y, 1)
  | otherwise = Left "Can't apply axiom"

rule1 :: Proof -> Proof -> Prove Proof
rule1 (Triple (p, x, a)) (Single q)
  | modExp x ((p - 1) `div` q) p /= 1 && q `divides` (p - 1) =
    return $ Triple (p, x, q * a)
rule1 _ _ = Left "Can't apply rule1"

rule2 :: Proof -> Prove Proof
rule2 (Triple (p, x, p'))
  | p' == p - 1 && modExp x p' p == 1 = return $ Single p
rule2 _ = Left "Can't apply rule2"

modExp :: Integer -> Integer -> Integer -> Integer
modExp x y m = go x y 1
  where
    go x 0 r = r
    go x y r =
      let x' = mod (x * x) m
      in case y `divMod` 2 of
           (y', 1) -> go x' y' (r * x `mod` m)
           (y', _) -> go x' y' r

divides :: Integer -> Integer -> Bool
divides n m = m `mod` n == 0
