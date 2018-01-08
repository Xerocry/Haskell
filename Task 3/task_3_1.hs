data WeirdPeanoNumber  = Zero | Succ (WeirdPeanoNumber ) | Pred (WeirdPeanoNumber ) deriving Show

simplefyFirst :: WeirdPeanoNumber  -> WeirdPeanoNumber 
simplefyFirst (Zero) = Zero
simplefyFirst (Succ (Pred x)) = simplefyFirst x
simplefyFirst (Pred (Succ x)) = simplefyFirst x
simplefyFirst (Succ x) = Succ (simplefyFirst x)
simplefyFirst (Pred x) = Pred (simplefyFirst x)

isSimple :: WeirdPeanoNumber  -> Bool
isSimple Zero = True
isSimple (Pred (Succ x)) = False
isSimple (Succ (Pred x)) = False
isSimple (Pred x) = isSimple x
isSimple (Succ x) = isSimple x

simplify :: WeirdPeanoNumber  -> WeirdPeanoNumber 
simplify x = let simplefied = simplefyFirst x in
    if (isSimple simplefied) then simplefied
    else simplify simplefied

eqSimple :: WeirdPeanoNumber  -> WeirdPeanoNumber  -> Bool
eqSimple Zero Zero = True
eqSimple _ Zero = False
eqSimple Zero _ = False
eqSimple (Succ x) (Pred y) = False
eqSimple (Pred y) (Succ x) = False
eqSimple (Succ x) (Succ y) = eqSimple x y
eqSimple (Pred x) (Pred y) = eqSimple x y

leqSimple :: WeirdPeanoNumber  -> WeirdPeanoNumber  -> Bool
leqSimple Zero Zero = True
leqSimple Zero (Succ _) = True
leqSimple Zero (Pred _) = False
leqSimple (Succ _) Zero = False
leqSimple (Pred _) Zero = True
leqSimple (Succ left) (Succ right) = left `leqSimple` right
leqSimple (Succ left) (Pred right) = False
leqSimple (Pred left) (Succ right) = True
leqSimple (Pred left) (Pred right) = left `leqSimple` right

signumSimple :: WeirdPeanoNumber  -> WeirdPeanoNumber 
signumSimple Zero = Zero
signumSimple (Succ _) = Succ Zero
signumSimple (Pred _) = Pred Zero

divSimple :: WeirdPeanoNumber  -> WeirdPeanoNumber  -> WeirdPeanoNumber 
divSimple x y = let t = x - y in
    if (t < 0)
        then Zero
        else (Succ (divSimple t y))

instance Eq WeirdPeanoNumber  where
    (==) left right = eqSimple (simplify left) (simplify right)

instance Ord WeirdPeanoNumber  where
    (<=) left right = leqSimple (simplify left) (simplify right) 

instance Num WeirdPeanoNumber  where
    (+) Zero right = right
    (+) left Zero = left
    (+) (Succ left) right = Succ (left + right)
    (+) (Pred left) right = Pred (left + right)
    
    negate Zero = Zero
    negate (Succ x) = Pred (negate x)
    negate (Pred x) = Succ (negate x) 
    
    fromInteger x | x == 0 = Zero
        | x < 0 = Pred (fromInteger (x + 1))
        | otherwise = Succ (fromInteger (x - 1))

    signum x = signumSimple (simplify x)
    
    abs x = if (signum x < Zero) then negate x else x

    (*) Zero _ = Zero
    (*) _ Zero = Zero
    (*) (Succ left) right = right + (left * right)
    (*) (Pred left) right = left*right + (negate right)

instance Enum WeirdPeanoNumber  where
    toEnum x | x == 0 = Zero
     | x < 0 = Pred (toEnum $ x + 1)
     | otherwise = Succ (toEnum $ x - 1) 
    fromEnum Zero = 0
    fromEnum (Succ left) = (fromEnum left) + 1
    fromEnum (Pred left) = (fromEnum left) - 1
 
instance Integral WeirdPeanoNumber  where
    quotRem left right = let isNeg = (signum left) == (signum right) in
        let div = divSimple (abs left) (abs right) in
        if (isNeg) then (div, simplify $ left - div * right) else (negate div, simplify $ left - div * right)

    toInteger Zero = 0
    toInteger (Succ left) = (toInteger left) + 1
    toInteger (Pred left) = (toInteger left) - 1

instance Real WeirdPeanoNumber  where
    toRational x = toRational (toInteger x)