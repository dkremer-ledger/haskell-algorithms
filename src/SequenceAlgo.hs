module SequenceAlgo
    ( sequenceOf
    ) where



-- | sequenceOf takes a value of type a, a list of type a elements
-- and return two lists. The first one is the first contiguous list
-- made of this element taken from the input argument, and the second
-- one is the remaining list
sequenceOf :: Eq a => a -> [a] -> ([a], [a]) 
sequenceOf = sequenceOf' []

sequenceOf' :: Eq a => [a] -> a -> [a] -> ([a], [a])
sequenceOf' acc x (y:ys)
  | x == y = sequenceOf' (y:acc) x ys
  | null acc && x /= y = sequenceOf' acc x ys
  | otherwise = (acc, (y:ys))

 
