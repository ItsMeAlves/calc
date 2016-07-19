-- module Expression(Expression(..), evaluate, fromPrefixNotation) where

import Data.Char

data Operation = Add | Subtract | Multiply | Divide deriving (Show, Eq)
data Expression = Empty | Value Float | Node Operation Expression Expression deriving (Show, Eq)

precedence :: Char -> Integer
precedence '+' = 1
precedence '-' = 1
precedence '*' = 2
precedence '/' = 2

hasFloat :: String -> Bool
hasFloat s = not $ null $ (reads s :: [(Float,String)])

extractFloat :: String -> Float
extractFloat s = fst $ head (reads s :: [(Float,String)])

plant :: [String] -> [Expression]
plant [] = []
plant (h:t)
    | hasFloat h = (Value $ extractFloat h):plant t
    | otherwise = (Node (toOperation $ head h) Empty Empty):plant t
    where toOperation c
            | c == '+' = Add
            | c == '-' = Subtract
            | c == '*' = Multiply
            | c == '/' = Divide
            | otherwise = error "Unsupported operation"


hasEmpty :: Expression -> Bool
hasEmpty Empty = True
hasEmpty (Value _) = False
hasEmpty (Node _ l r) = (hasEmpty l) || (hasEmpty r)

insertInto :: Expression -> Expression -> Expression
insertInto Empty item = item
insertInto (Value x) _ = Value x
insertInto (Node o l r) item
    | hasEmpty l = Node o (insertInto l item) r
    | hasEmpty r = Node o l (insertInto r item)
    | otherwise = (Node o l r)

evaluate :: Expression -> Float
evaluate (Value x) = x
evaluate (Node Add x y) = evaluate x + evaluate y
evaluate (Node Subtract x y) = evaluate x - evaluate y
evaluate (Node Multiply x y) = evaluate x * evaluate y
evaluate (Node Divide x y) = evaluate x / evaluate y
evaluate Empty = error "This is an incomplete expression"

fromPrefixNotation :: String -> Expression
fromPrefixNotation s = foldl (\acc e -> insertInto acc e) Empty $ plant $ words s


