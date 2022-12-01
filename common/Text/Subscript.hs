module Text.Subscript
  ( subscript
  ) where

subscript :: Int -> String
subscript = map sub . show
  where
    sub '0' = '₀'
    sub '1' = '₁'
    sub '2' = '₂'
    sub '3' = '₃'
    sub '4' = '₄'
    sub '5' = '₅'
    sub '6' = '₆'
    sub '7' = '₇'
    sub '8' = '₈'
    sub '9' = '₉'
    sub c = c
