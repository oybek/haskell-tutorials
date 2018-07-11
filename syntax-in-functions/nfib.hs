
mul :: (Num a) => (a, a, a, a) -> (a, a, a, a) -> (a, a, a, a)
mul (x11, x12, x21, x22) (y11, y12, y21, y22) = 
  ( x11*y11 + x12*y21
  , x11*y12 + x12*y22
  , x21*y11 + x22*y21
  , x21*y12 + x21*y22 )

fib :: (Integral a) => a -> a
pow' :: (Integral a) => a -> (a, a, a, a)

-- Gets first element of tuple
first (a, _, _, _) = a

-- Calc fibonacci in O(n)
fib 0 = 0
fib 1 = 1
fib n = first (pow' (n-1))

--
pow' 1 = (1, 1, 1, 0)
pow' n = mul (pow' (n-1)) (pow' 1)

-- Calc fibonacci in O(log n)
fibLog 0 = 0
fibLog 1 = 1
fibLog n = first (powLog' (n-1))

--
powLog n
  | n == 0 = []
  | otherwise = [mod n 2] ++ powLog (div n 2)

--
powLog' 0 = (1, 1, 1, 0)
powLog' n = let t = powLog' (n-1) in mul t t

