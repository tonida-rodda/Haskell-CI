module Lib (module Lib) where


data Cpt = Value Int | Symbol String | List [Cpt]
    deriving ( Eq, Show )

data Sym = Sint Int | SBool Bool | Sstring String | Schar Char
    deriving ( Eq, Show )

data Ast = Define String Ast
    | Call Ast [Ast]
    | Aint Int
    | Astring String
    | Abool Bool
    deriving (Eq, Show)

testCustomType :: Int -> Sym
testCustomType x = Sint x

declareSymbols :: String -> Cpt
declareSymbols x = Symbol x

declareValue :: Int -> Cpt
declareValue x = Value x

declareList :: Int -> Cpt
declareList x = (List [Value x])

getSymbol :: Cpt -> Maybe String
getSymbol (Symbol x) = Just x
getSymbol _ = Nothing

getValue :: Cpt -> Maybe Int
getValue (Value x) = Just x
getValue _ = Nothing

getLists :: Cpt -> Maybe [Cpt]
getLists (List x) = Just x
getLists _ = Nothing

mbsts :: Maybe String -> String
mbsts Nothing = ""
mbsts (Just x) = x

printTree :: Cpt -> Maybe String
printTree (Value x) = Just ("Value "++show x)
printTree (Symbol x) = Just ("Symbols "++show x)
printTree (List x) = Just (unwords (map mbsts (map printTree x)))

cptToAST :: Cpt -> Maybe Ast
cptToAST (List []) = Nothing
cptToAST (List [l]) = cptToAST l
cptToAST (Value x) = Just (Aint x)
cptToAST (Symbol x) = Just (Astring x)
cptToAST (List [(Symbol "define"), (Symbol x), (Value s)]) = Just (Define x (Aint s))
cptToAST (List [(Symbol "define"), (Symbol x), (Symbol "#t")]) = Just (Define x (Abool True))
cptToAST (List [(Symbol "define"), (Symbol x), (Symbol "#f")]) = Just (Define x (Abool False))
cptToAST (List [(Symbol "define"), (Symbol x), (Symbol s)]) = Just (Define x (Astring s))
cptToAST (List l) = sequence (map cptToAST l) >>= (\(x:xs) -> Just (Call x xs))
cptToAST _ = Nothing

myReverse :: [a] -> [a]
myReverse [] = []

someFunc :: IO ()
someFunc = putStrLn "la"