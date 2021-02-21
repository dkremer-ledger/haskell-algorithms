module Arithmetic (
  sum0,
  sum1,
  product1
) where


import qualified Prelude

sum0 :: (Prelude.Num a, Prelude.Foldable t) => t a -> a
sum0 = Prelude.foldr (Prelude.+) 0

sum1 [] = 0
sum1 (x:xs) = x (Prelude.+) (sum1 xs)

product1 [] = 1
product1 (x:xs) = x (Prelude.*) (product1 xs)


