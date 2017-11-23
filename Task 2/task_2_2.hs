foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' f x0 []      =  x0
foldr' f x0 (x:xs)  =  f x (foldr' f x0 xs)
 
foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' f x0 []      =  x0
foldl' f x0 (x:xs)  =  foldl' f (f x0 x) xs

map :: (a -> b) -> [a] -> [b]  
map f  =  foldr ((:) . f) []

flatMap :: (a -> [b]) -> [a] -> [b]  
flatMap _ [] = []
flatMap f list = foldr (\h t -> (f h) ++ t) [] list

concat :: [a] -> [a] -> [a]  
concat [] [] = []
concat list1 list2 = foldr (\h t -> h:t) list2 list1

filter :: (a -> Bool) -> [a] -> [a]  
filter f  =  foldr (\ x xs -> if f x then x : xs else xs) []

maxBy :: (a -> Integer) -> [a] -> a  
maxBy f (h:t) = foldl (\max h -> if (f max) > (f h) then max else h) h t

minBy :: (a -> Integer) -> [a] -> a  
minBy f (h:t) = foldl (\min h -> if (f min) < (f h) then min else h) h t

reverse :: [a] -> [a]  
reverse = foldl (flip (:)) []

elementAt :: Integer -> [a] -> a  
elementAt _ [] = error "list is empty"
elementAt x list | ((length list) <= (fromIntegral x)) = error "no element at list"
    | otherwise = res
      where (res, _) = foldl (\(res,x) h -> if (x == 0) then (h, x-1) else (res, x-1)) (head list, x) list

indexOf :: String -> [String] -> Integer   
indexOf _ [] = error "list is empty"
indexOf x list = 
    if (res == -1) 
        then error "element not found" 
    else res
        where (res, _, _) = foldl (\(res, count, x) h -> if (h == x) then (count, count+1, x) else (res, count+1, x)) (-1, 0, x) list
        