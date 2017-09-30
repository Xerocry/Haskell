{-
data Term = IntConstant{ intValue :: Int }
            | Variable{ varName :: String }
            | BinaryTerm{ lhv :: Term, rhv :: Term } deriving(Show,Eq)
Данная структура данных представляет собой ячейку дерева разбора некоторого языка, содержащего в себе числовые константы, переменные и бинарные операторы.
Следует расширить данную структуру данных таким образом,
чтобы она позволяла описывать различные бинарные операторы (сложение, умножение, вычитание), а так же унарный минус.
Помимо этого, нужно описать ряд функций:
  * Операторы <+>, <-> и <*>, которые создают значения соответствующих бинарных операций.
  * Функцию замены replaceVar, которая принимает имя переменной и терм, на который её следует заменить, и производит замену этой переменной на этот терм по всему выражению.
-}

data Operator = Plus | Minus | Mult deriving (Show, Eq)

data Term = IntConstant{ intValue :: Int }
            | Variable{ varName :: String }
            | UnaryTerm{ trm :: Term, op :: Operator }
            | BinaryTerm{ lhv :: Term, rhv :: Term, op :: Operator }
            deriving (Show, Eq)

a <+> b = BinaryTerm a b Plus
a <-> b = BinaryTerm a b Minus
a <*> b = BinaryTerm a b Mult
(-) a = UnaryTerm a Minus


replaceVar :: Term -> String -> Term -> Term
replaceVar (IntConstant intConst) _ _ = IntConstant intConst
replaceVar (Variable varConst) value newTerm = if varConst == value then newTerm else Variable varConst
replaceVar (UnaryTerm op unConst) value newTerm = UnaryTerm op (replaceVar unConst value newTerm)
replaceVar (BinaryTerm left right operator) value newTerm = 
    BinaryTerm (replaceVar left value newTerm) (replaceVar right value newTerm) operator