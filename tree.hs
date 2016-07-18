import Data.Char

data Operation = Add | Subtract | Multiply | Divide deriving (Show, Eq)
data Expression = Empty | Value Int | Node Operation Expression Expression deriving (Show, Eq)

plant :: String -> [Expression]
plant [] = []
plant (h:t)
    | isDigit h = (Value $ digitToInt h):plant t
    | otherwise = (Node (toOperation h) Empty Empty):plant t
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

eval :: Expression -> Int
eval (Value x) = x
eval (Node Add x y) = eval x + eval y
eval (Node Subtract x y) = eval x - eval y
eval (Node Multiply x y) = eval x * eval y
-- eval (Node Divide x y) = eval x / eval y
eval _ = error "Invalid expression"

-- main = do
--     let expression = foldl (\acc e -> insertInto acc e) Empty $ plant "***2222"
--     print $ eval expression
