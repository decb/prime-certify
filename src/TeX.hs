{-# LANGUAGE ViewPatterns #-}

module TeX
  ( writeDerivation
  ) where

import Data.List (intercalate)

import Prime

writeDerivation :: FilePath -> Proof -> IO ()
writeDerivation fp proof = do
  let content = document (proofToTex proof)
  writeFile fp content

document :: String -> String
document body =
  unlines
    [ "\\documentclass{standalone}"
    , "\\usepackage{amsmath}"
    , "\\begin{document}"
    , "$"
    , body
    , "$"
    , "\\end{document}"
    ]

proofToTex :: Proof -> String
proofToTex p = derivation
  where
    val =
      case value p of
        (Triple x y z) -> "(" <> commas (map show [x, y, z]) <> ")"
        (Single x) -> show x
    derivation =
      case back p of
        (Axiom (show -> x) (show -> y)) -> derive "" (commas [x, y]) val "A"
        (Rule1 i j) ->
          case (value i, value j) of
            (Triple (show -> p) (show -> x) (show -> a), Single (show -> q)) ->
              derive
                (proofToTex i <> "\\qquad" <> proofToTex j)
                (pow x ("(" <> p <> " - 1) / " <> q) <> " \\neq 1" <> modulo p)
                val
                ("R" <> sub "1")
            _ -> error "Impossible: Rule1"
        (Rule2 i) ->
          case value i of
            (Triple (show -> p) (show -> x) (show -> p')) ->
              derive
                (proofToTex i)
                (pow x (p <> " - 1") <> " = 1" <> modulo p)
                val
                ("R" <> sub "2")
            _ -> error "Impossible: Rule2"

pow :: String -> String -> String
pow x y = x <> "^{" <> y <> "}"

sc :: String -> String
sc s = "\\textsc{" <> s <> "}"

derive :: String -> String -> String -> String -> String
derive top lhs rhs name =
  concat ["\\dfrac{", top, "}{", lhs, "\\vdash", rhs, "}\\; ", sc name]

modulo :: String -> String
modulo n = "(\\mathrm{mod}\\ " <> n <> ")"

commas :: [String] -> String
commas = intercalate ",\\, "

sub :: String -> String
sub s = "\\textsubscript{" <> s <> "}"
