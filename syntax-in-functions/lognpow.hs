
-- log n can't be realized using only 'pattern matching'
pow :: (Num a, Integral n) => a -> n -> a
pow a 0 = 1
pow a n =
  if (mod n 2) == 0
  then t*t
  else a * pow a (n-1)
  where t = pow a (div n 2)

-- Using guard 'Guards'
pow1 :: (Num a, Integral n) => a -> n -> a
pow1 a n
  | n == 0 = 1
  | n `mod` 2 == 0 = t * t
  | otherwise = a * pow1 a (n-1)
  where t = pow1 a (div n 2)

-- Using 'let in' and guards
pow2 :: (Num a, Integral n) => a -> n -> a
pow2 a n
  | n == 0 = 1
  | n `mod` 2 == 0 =
    let t = pow a (div n 2)
    in t * t
  | otherwise = a * pow a (n-1)

-- Using case
pow3 :: (Num a, Integral n) => a -> n -> a
pow3 a n = case n of
  0 -> 1
  n ->
    if (mod n 2) == 0
    then t*t
    else a * pow3 a (n-1)
  where t = pow a (div n 2)

