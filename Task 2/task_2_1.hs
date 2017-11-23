data BinaryTree = EmptyTree | Node Integer BinaryTree BinaryTree deriving (Show, Read, Eq)

emptyTree :: BinaryTree
emptyTree = EmptyTree

treeInsert :: BinaryTree -> Integer -> BinaryTree
treeInsert EmptyTree x = Node x EmptyTree EmptyTree  
treeInsert (Node a left right) x    
    | x == a = Node x left right  
    | x < a  = Node a (treeInsert left x) right  
    | x > a  = Node a left (treeInsert right x)

treeRemove :: BinaryTree -> Integer -> BinaryTree
treeRemove EmptyTree _ = EmptyTree
treeRemove (Node a left right) x 
  | x < a = Node a (treeRemove right x) right
  | x > a = Node a left (treeRemove right x)
  | x == a = 
    if (left == EmptyTree)
      then right
    else if (right == EmptyTree)
      then left
    else if ((left /= EmptyTree) && (right /= EmptyTree))
      then (Node (leftMostElement right) left right)
    else error "treeRemove :: Эта ветка никогда не должна выполняться!!!"
      where 
          leftMostElement (Node value EmptyTree _) = value
          leftMostElement (Node _ t1 _) = leftMostElement t1    

containsElement :: BinaryTree -> Integer -> Bool
containsElement EmptyTree _ = False
containsElement (Node a left right) x 
    | x == a = True
    | x < a  = containsElement left x 
    | x > a  = containsElement right x 


treeFromList :: [Integer] -> BinaryTree
treeFromList [] = EmptyTree
treeFromList (x:xs) = treeFromList' (Node x EmptyTree EmptyTree) xs
  where
    treeFromList' tr [] = tr
    treeFromList' tr (x:xs) = treeFromList' (treeInsert tr x) xs

listFromTree :: BinaryTree -> [Integer]
listFromTree tree = listFromTree' tree []
  where 
    listFromTree' EmptyTree list = list
    listFromTree' (Node a left right) list = listFromTree' left (a : (listFromTree' right list))


nearestGE :: BinaryTree -> Integer -> Integer
nearestGE EmptyTree x = error "Empty Tree" 
nearestGE (Node a EmptyTree right) x 
  | (a == x) = a
  | (a < x) = nearestGE right x
  | otherwise = a                  
nearestGE (Node a left EmptyTree) x        
  | (a == x) = a
  | (a > x) = if (nearestLeft >= x) then nearestLeft else a
  | otherwise = a
    where nearestLeft = nearestGE left x
nearestGE (Node a left right) x 
  | (a == x) = a
  | (a > x) = if (nearestLeft >= x) then nearestLeft else a
  | (a < x) = nearestGE right x
    where nearestLeft = nearestGE left x