module Parsing (module Parsing) where



newtype Parser a = Parser {
    runParser :: String -> Maybe (a, String)
}

parseChar :: Char -> Parser Char
parseChar c = Parser {
    runParser = \(x:xs) -> case () of
                            _ | c == x -> Just (x, xs)
                              | otherwise -> Nothing
}

parseAnyChar :: String -> Parser Char
parseAnyChar s = Parser {
    runParser = \(x:xs) -> case () of
                            _ |  x `elem` s -> Just (x, xs)
                              | otherwise -> Nothing
}

parseOr :: Parser a -> Parser a -> Parser a
parseOr f1 f2 = Parser {
    runParser = \xs -> case f1 xs of
                    Just n -> Just n
                    Nothing -> f2 xs
}

-- parseAnd :: Parser a -> Parser b -> Parser (a, b)
-- parseAnd f1 f2 x = case f1 x of
--                     Just (v, n) -> f2 n >>= (\(z, xs) -> Just ((v, z), xs))
--                     _ -> Nothing

-- parseAndWith :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
-- parseAndWith fWith f1 f2 x = parseAnd f1 f2 x >>= (\((v, z), xs) -> Just (fWith v z, xs))

-- parseMany :: Parser a -> Parser [a]
-- parseMany _ [] = Just ([], [])
-- parseMany f1 xs = case f1 xs of
--                  Just (v, n) -> parseMany f1 n >>= (\(list, xs2) -> Just (v:list, xs2))
--                  _ -> Just ([], xs)

-- parseSome :: Parser a -> Parser [a]
-- parseSome _ [] = Nothing
-- parseSome f1 (x:xs) = case f1 [x] of
--                  Just (v, n) -> parseMany f1 xs >>= (\(list, xs2) -> Just (v:list, xs2))
--                  _ -> Nothing

-- parseUInt :: Parser Int
-- parseUInt str = parseSome (parseAnyChar ['0'..'9']) str >>= (\(list, xs) -> Just (read list :: Int, xs))

-- parseInt :: Parser Int
-- parseInt ('-':str) = parseUInt str >>= (\(nb, xs) -> Just (negate nb, xs))
-- parseInt str = parseUInt str

-- parseList :: Parser a -> Parser [a]
-- parseList f1 (x:str)
--     | x == '(' || x == ' ' = case f1 str of
--         Just (v, xs) -> parseList f1 xs >>= (\(list, xs2) -> Just (v:list, xs2))
-- parseList f1 (')':str) = Just ([], str)
-- parseList _ _ = Nothing