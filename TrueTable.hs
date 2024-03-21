module TrueTable where

data Expr = Var Char
    | Not (Maybe Expr)
    | And (Maybe Expr) (Maybe Expr)
    | Or (Maybe Expr) (Maybe Expr)
    deriving (Show)

removeParentheses :: String -> String
removeParentheses = filter (not . isParenthesis)
    where
    isParenthesis :: Char -> Bool
    isParenthesis c = c `elem` ['(', ')']a

parserExpr :: String -> Maybe Expr
parserExpr [] = Nothing
parserExpr str = parserExpr' (removeParentheses str)
    where
    parserExpr' [] = Nothing
    parserExpr' [x] = Just (Var x)
    parserExpr' str'
        | dropuntilsimbol str' '^' /= [] = parserExprVal (takeuntilsimbol str' '^') (dropuntilsimbol str' '^')
        | dropuntilsimbol str' 'v' /= [] = parserExprVal (takeuntilsimbol str' 'v') (dropuntilsimbol str' 'v')
        | dropuntilsimbol str' '~' /= [] = parserExprVal (takeuntilsimbol str' '~') (dropuntilsimbol str' '~')
        | otherwise = Nothing

parserExprVal :: String -> String -> Maybe Expr
parserExprVal _ [] = Nothing
parserExprVal [] (x:xs) | x == '~' = Just (Not (parserExpr xs))
                        | otherwise = Nothing
parserExprVal str (x:xs) | x == 'v' = Just (Or (parserExpr str) (parserExpr xs))
                          | x == '^' = Just (And (parserExpr str) (parserExpr xs))

takeuntilsimbol :: String -> Char -> String
takeuntilsimbol [] _ = []
takeuntilsimbol str c = takeWhile (/= c) str

dropuntilsimbol :: String -> Char -> String
dropuntilsimbol [] _ = []
dropuntilsimbol str c = dropWhile (/= c) str

solver :: String -> Maybe Bool
solver [] = Nothing
solver [x] | x == '^' = Nothing
           | x == 'v' = Nothing
           | x == '~' = Nothing
           | otherwise = Just (solveExpr (parserExpr (show x)))
solver str = Just (solveExpr (parserExpr str))

solveExpr :: Maybe Expr -> Bool
solveExpr Nothing = False
solveExpr (Just (Or ex1 ex2)) = (solveExpr ex1 || solveExpr ex2)
solveExpr (Just (And ex1 ex2)) = (solveExpr ex1 && solveExpr ex2)
solveExpr (Just (Not ex1)) = (not (solveExpr ex1))
solveExpr (Just (Var ex1)) | ex1 == 'a' = True
                           | ex1 == 'b' = True
                           | ex1 == 'c' = False
                           | otherwise = False
                           