import Calculate.Expression

main = do
    putStrLn "Oh, hi! Enter a expression and I will evaluate it!"
    expression <- getLine
    putStrLn "..."
    putStrLn "This input results in:"
    print $ evaluate $ fromPrefixNotation expression