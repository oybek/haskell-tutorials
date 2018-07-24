
-- Pattern matching O(2^n)
-- TODO: how to memoize using 'where'?
fib :: (Integral a) => a -> a
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

-- Guards
fib2 :: (Integral a) => a -> a
fib2 n
  | n == 0 = 0
  | n == 1 = 1
  | otherwise = fib2 (n-1) + fib2 (n-2)

-- Let in
fib3 :: (Integral a) => a -> a
fib3 0 = 0
fib3 1 = 1
fib3 n =
  let t1 = fib3 (n-1)
      t2 = fib3 (n-2)
  in t1 + t2

-- Case
fib4 :: (Integral a) => a -> a
fib4 n = case n of
  0 -> 0
  1 -> 1
  n -> fib4 (n-1) + fib4 (n-2)

