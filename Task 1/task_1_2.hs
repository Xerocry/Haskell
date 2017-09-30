myNod :: Int -> Int -> Int
myNod 0 0 = error "Both are zeros"
myNod m 0 = abs m
myNod 0 n = abs n
myNod m n 
    | m == n       = m 
    | m > n        = myNod n (rem m n) --works with negative
    | otherwise    = myNod n m


{-
Algorythm : 
https://en.wikipedia.org/wiki/Exponentiation_by_squaring
-}
myPow :: Integer -> Integer -> Integer
myPow x y
    | y < 0 = error "negative power"
    | y == 0 = 1
    | y == 1 = x
    | otherwise = pow' y x
    where
        pow' kth num
            | odd kth = num * pow' (kth `quot` 2) (num * num) --quot rounds towards zero!
            | otherwise = pow' (kth `quot` 2) (num * num)


mySin :: Integer->Float->Float->Float
mySin k x result = 
  if k == 130 then
   result + ( (((-1)^k ) * ((x^((2*k)+1))) / (fromIntegral $ product [1..2*k+1])))
  else
   result + (mySin (k+1) x ( (((-1)^k ) * ((x^((2*k)+1))) / (fromIntegral $ product [1..2*k+1]))))
