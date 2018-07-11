
mul :: (Num a) => (a, a, a, a) -> (a, a, a, a) -> (a, a, a, a)
mul (x11, x12, x21, x22) (y11, y12, y21, y22) = 
  ( x11*y11 + x12*y21
  , x11*y12 + x12*y22
  , x21*y11 + x22*y21
  , x21*y12 + x21*y22 )

