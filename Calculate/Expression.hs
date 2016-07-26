module Calculate.Expression(Expression(..), 
    evaluate, 
    fromPrefixNotation, 
    fromPostfixNotation) 
    -- fromInfixNotation) 
where

-- Define all supported operations and the Expression constructors
-- Empty means a space to be replaced by a Value or a Node
-- Value means a float number
-- Node means a Operation between two expressions
data Operation = Add | Subtract | Multiply | Divide | Pow deriving (Show, Eq)
data Expression = Empty | Value Float | Node Operation Expression Expression deriving (Show, Eq)

-- Check if a given Expression is constructed by Value
isValue :: Expression -> Bool
isValue (Value _) = True
isValue _ = False

-- Check if a given Expression is constructed by Node
isNode :: Expression -> Bool
isNode (Node _ _ _) = True
isNode _ = False

-- Tells the precedence order
precedence :: Operation -> Integer
precedence Add = 1
precedence Subtract = 1
precedence Multiply = 2
precedence Divide = 2
precedence Pow = 3

-- It receives a expression and replaces its first empty cell by another given expression
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

-- Reads the string input and translates it into small expressions 
plant :: [String] -> [Expression]
plant [] = []
plant (h:t)
    | hasFloat h = (Value $ extractFloat h):plant t
    | otherwise = (Node (toOperation $ head h) Empty Empty):plant t
    where hasFloat s = not $ null (reads s :: [(Float,String)])
          extractFloat s = fst $ head (reads s :: [(Float,String)])
          toOperation c
            | c == '+' = Add
            | c == '-' = Subtract
            | c == '*' = Multiply
            | c == '/' = Divide
            | c == '^' = Pow
            | otherwise = error "Unsupported operation"

-- Receives an expression and returns its result
evaluate :: Expression -> Float
evaluate (Value x) = x
evaluate (Node Add x y) = evaluate x + evaluate y
evaluate (Node Subtract x y) = evaluate x - evaluate y
evaluate (Node Multiply x y) = evaluate x * evaluate y
evaluate (Node Divide x y) = evaluate x / evaluate y
evaluate (Node Pow x y) = evaluate x ** evaluate y
evaluate Empty = error "This is an incomplete expression"

-- Get small expressions and construct the full expression
generate :: [Expression] -> Expression
generate s = foldl (\acc e -> insertInto acc e) Empty s

-- Convert from postfix notation to prefix notation
fromPostfixToPrefix :: [Expression] -> [Expression] -> [Expression]
fromPostfixToPrefix [] result = result
fromPostfixToPrefix (a:b) stack@(x:y:z)
    | isValue a = fromPostfixToPrefix b (a:stack)
    | isNode a = fromPostfixToPrefix b ((insertInto (insertInto a y) x):z)
fromPostfixToPrefix (a:b) stack
    | isValue a = fromPostfixToPrefix b (a:stack)
    | isNode a = error "invalid postfix expression"

-- Receives a string input and translate it into small expressions
prepareInput :: String -> [Expression]
prepareInput s = plant $ words s

-- Receives a string input in prefix notation and returns the expression
fromPrefixNotation :: String -> Expression
fromPrefixNotation s = generate $ prepareInput s

-- Receives a string input in postfix notation and returns the expression
fromPostfixNotation :: String -> Expression
fromPostfixNotation s = generate $ fromPostfixToPrefix (prepareInput s) []

-- fromInfixNotation :: String -> Expression
-- fromInfixNotation s = generate $ fromPostfixToPrefix (toPostfix (prepareInput s))
--     where toPostfix t = t
    -- where toPostfix [] [] result = reverse result
    --       toPostfix [] (a:b) s = toPostfix [] b (a:s)
    --       toPostfix (a:b) stack@(x:y) s


