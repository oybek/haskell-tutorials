
-- Realization of pow using 'pattern matching'
pow :: (Num a, Integral n) => a -> n -> a
pow a 0 = 1
pow a n = a * pow a (n-1)

-- Realization of pow using 'guards'
pow2 :: (Num a, Integral n) => a -> n -> a
pow2 a n
  | n == 0 = 1
  | otherwise = a * pow2 a (n-1)

-- Realization of pow using 'let ... in ...' construction
pow3 :: (Num a, Integral n) => a -> n -> a
pow3 a 0 = 1
pow3 a n =
  let t = pow3 a (n-1)
  in a * t

-- Realization of pow using 'case ... of ...' construction
pow4 :: (Num a, Integral n) => a -> n -> a
pow4 a n = case n of
  0 -> 1
  n -> a * pow4 a (n-1)

