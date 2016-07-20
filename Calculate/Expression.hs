module Calculate.Expression(Expression(..), 
    evaluate, 
    fromPrefixNotation, 
    fromPostfixNotation, 
    fromInfixNotation) 
where

data Operation = Add | Subtract | Multiply | Divide | Pow deriving (Show, Eq)
data Expression = Empty | Value Float | Node Operation Expression Expression deriving (Show, Eq)

isValue :: Expression -> Bool
isValue (Value _) = True
isValue _ = False

isNode :: Expression -> Bool
isNode (Node _ _ _) = True
isNode _ = False

precedence :: Operation -> Integer
precedence Add = 1
precedence Subtract = 1
precedence Multiply = 2
precedence Divide = 2
precedence Pow = 3

insertInto :: Expression -> Expression -> Expression
insertInto Empty item = item
insertInto (Value x) _ = Value x
insertInto (Node o l r) item
    | hasEmpty l = Node o (insertInto l item) r
    | hasEmpty r = Node o l (insertInto r item)
    | otherwise = (Node o l r)
    where hasEmpty Empty = True
          hasEmpty (Value _) = False
          hasEmpty (Node _ l r) = (hasEmpty l) || (hasEmpty r)

plant :: [String] -> [Expression]
plant [] = []
plant (h:t)
    | hasFloat h = (Value $ extractFloat h):plant t
    | otherwise = (Node (toOperation $ head h) Empty Empty):plant t
    where hasFloat s = not $ null $ (reads s :: [(Float,String)])
          extractFloat s = fst $ head (reads s :: [(Float,String)])
          toOperation c
            | c == '+' = Add
            | c == '-' = Subtract
            | c == '*' = Multiply
            | c == '/' = Divide
            | c == '^' = Pow
            | otherwise = error "Unsupported operation"

evaluate :: Expression -> Float
evaluate (Value x) = x
evaluate (Node Add x y) = evaluate x + evaluate y
evaluate (Node Subtract x y) = evaluate x - evaluate y
evaluate (Node Multiply x y) = evaluate x * evaluate y
evaluate (Node Divide x y) = evaluate x / evaluate y
evaluate (Node Pow x y) = evaluate x ** evaluate y
evaluate Empty = error "This is an incomplete expression"

generate :: [Expression] -> Expression
generate s = foldl (\acc e -> insertInto acc e) Empty s

postfixBuilder :: [Expression] -> Expression
postfixBuilder s = generate $ toPrefix s []
    where toPrefix [] result = result
          toPrefix (a:b) stack@(x:y:z)
            | isValue a = toPrefix b (a:stack)
            | isNode a = toPrefix b ((insertInto (insertInto a y) x):z)
          toPrefix (a:b) stack
            | isValue a = toPrefix b (a:stack)
            | isNode a = error "invalid postfix expression"

prepareInput :: String -> [Expression]
prepareInput s = plant $ words s

fromPrefixNotation :: String -> Expression
fromPrefixNotation s = generate $ prepareInput s

fromPostfixNotation :: String -> Expression
fromPostfixNotation s = postfixBuilder $ prepareInput s

fromInfixNotation :: String -> Expression
fromInfixNotation s = postfixBuilder $ toPostfix (prepareInput s)
    where toPostfix t = t
    -- where toPostfix [] [] result = reverse result
    --       toPostfix [] (a:b) s = toPostfix [] b (a:s)
    --       toPostfix (a:b) stack@(x:y) s


