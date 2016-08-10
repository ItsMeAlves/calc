import Calculate.Expression
import Options.Applicative

-- Configure args structure
data Commander = Commander {
    expression :: String,
    prefix :: Bool,
    postfix :: Bool,
    infixFlag :: Bool
}

-- Help parsing args structures
options :: Parser Commander
options = Commander
    <$> strOption
        (long "expression"
            <> short 'e'
            <> help "The expression")
    <*> switch
        (long "prefix"
            <> help "Receives a prefix notation expression and evaluates it")
    <*> switch
        (long "postfix"
            <> help "Receives a postfix notation expression and evaluate")
    <*> switch
        (long "infix"
            <> help "Receives a infix notation expression and evaluates it")

-- Handle arguments
optionsHandler :: Commander -> IO ()
optionsHandler (Commander e True False False) = print $ evaluate $ fromPrefixNotation e
optionsHandler (Commander e False True False) = print $ evaluate $ fromPostfixNotation e
optionsHandler (Commander e False False True) = print $ evaluate $ fromInfixNotation e
optionsHandler (Commander _ _ _ _) = print "Error: Tell me its notation"

-- Main function
main = execParser opts >>= optionsHandler
    where opts = info (helper <*> options) (fullDesc
            <> progDesc "Calculates the result of an expression"
            <> header "Calculate - A simple CLI calculator")
