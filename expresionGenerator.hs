-- Typ reprezentující operaci uvnitř typu Exp
data Op = Plus | Minus | Times 
  deriving (Eq, Ord)

-- Typ reprezentující strom výpočtu. V listech máme celočíselnou konstantu, ve vnitřních vrcholech operace
data Exp = Const Int | Oper Op Exp Exp
  deriving (Eq, Ord)

instance Show Op where
    show Plus = "+"
    show Minus = "-"
    show Times = "*"

instance Show Exp where
    show (Const x) = show x
    show (Oper op (Const x1) (Const x2)) = show x1 ++ " " ++ show op ++ " " ++ show x2
    show (Oper op (Const x1) ex2) = show x1 ++ " " ++ show op ++ " (" ++ show ex2 ++ ")"
    show (Oper op ex1 (Const x2)) = "(" ++ show ex1 ++ ") " ++ show op ++ " " ++ show x2
    show (Oper op ex1 ex2) = "(" ++ show ex1 ++ ") " ++ show op ++ " (" ++ show ex2 ++ ")"

arit :: [Int] -> Int -> [Exp]
arit list result = [x | x <- (komb list), (eval x) == result]

komb :: [Int] -> [Exp] 
komb list = mergeWith (splitList list)

split :: Exp -> Exp -> [Exp]
split exp1 exp2 = [Oper Plus exp1 exp2, Oper Minus exp1 exp2, Oper Times exp1 exp2]

splitMulti :: [Exp] -> [Exp] -> [Exp]
splitMulti exp1 exp2 = concat [(split ex1 ex2) | ex1 <- exp1, ex2 <- exp2]

splitList :: [Int] -> [([Int], [Int])]
splitList list = [(take l list, drop l list) | l <- count]
    where
        count = [1..(length list - 1)]

mergeWith :: [([Int], [Int])] -> [Exp]
mergeWith [([x],[xs])] = split (Const x) (Const xs)
mergeWith list = concat (map (\ (x, xs) -> innerMerge x xs) list)
    where
        innerMerge [x] xs = splitMulti [Const x] (mergeWith (splitList xs))
        innerMerge x [xs] = splitMulti (mergeWith (splitList x)) [Const xs]
        innerMerge x xs = splitMulti (mergeWith (splitList x)) (mergeWith (splitList xs))

eval :: Exp -> Int
eval (Const x) = x
eval (Oper Plus exp1 exp2) = (eval exp1) + (eval exp2)
eval (Oper Minus exp1 exp2) = (eval exp1) - (eval exp2)
eval (Oper Times exp1 exp2) = (eval exp1) * (eval exp2)
