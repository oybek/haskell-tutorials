
-- Matrix multiplication
mul :: (Num a) => (a, a, a, a) -> (a, a, a, a) -> (a, a, a, a)
mul (x11, x12, x21, x22) (y11, y12, y21, y22) =
  ( x11*y11 + x12*y21
  , x11*y12 + x12*y22
  , x21*y11 + x22*y21
  , x21*y12 + x21*y22 )

-- Gets first element of tuple
first :: (Num a) => (a, a, a, a) -> a
first (a, _, _, _) = a

fib :: (Integral a) => a -> a
fib n = first (pow (1, 1, 1, 0) (n-1))

pow :: (Num m, Integral n) => (m, m, m, m) -> n -> (m, m, m, m)
pow (x11, x12, x21, x22) 0 = (1, 0, 0, 1)
pow (x11, x12, x21, x22) x =
  mul (pow (x11, x12, x21, x22) (x-1)) (x11, x12, x21, x22)

