module Examples where

import Prime

prime3 :: Prove Proof
prime3 = do
  a <- axiom 2 1
  b <- rule2 a
  c <- axiom 3 2
  d <- rule1 c b
  rule2 d

prime474397531 :: Prove Proof
prime474397531 = do
  d1 <- axiom 2 1
  d2 <- rule2 d1
  d3 <- axiom 3 2
  d4 <- rule1 d3 d2
  d5 <- rule2 d4
  d6 <- axiom 5 2
  d7 <- rule1 d6 d2
  d8 <- rule1 d7 d2
  d9 <- rule2 d8
  d10 <- axiom 251 6
  d11 <- rule1 d10 d2
  d12 <- rule1 d11 d9
  d13 <- rule1 d12 d9
  d14 <- rule1 d13 d9
  d15 <- rule2 d14
  d16 <- axiom 474397531 2
  d17 <- rule1 d16 d2
  d18 <- rule1 d17 d5
  d19 <- rule1 d18 d9
  d20 <- rule1 d19 d15
  d21 <- rule1 d20 d15
  d22 <- rule1 d21 d15
  rule2 d22

primeM107 :: Prove Proof
primeM107 = generate (2 ^ 107 - 1)
