import Calculate.Expression
import Options.Applicative

-- Configure args structure
data Commander = Commander {
    expression :: String,
    prefix :: Bool,
    postfix :: Bool
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
            <> help "Receives a postfix notation expression and evaluates it")

-- Handle arguments
optionsHandler :: Commander -> IO ()
optionsHandler (Commander e True False) = print $ evaluate $ fromPrefixNotation e
optionsHandler (Commander e False True) = print $ evaluate $ fromPostfixNotation e
optionsHandler (Commander e True True) = print result 
    where result = (zip ["From prefix: ", "fromPostfixNotation"]
            [evaluate $ fromPrefixNotation e, evaluate $ fromPostfixNotation e])
optionsHandler (_ False False) = print "Error: Tell me its notation"

-- Main function
main = execParser opts >>= optionsHandler
    where opts = info (helper <*> options) (fullDesc
            <> progDesc "Calculates the result of an expression"
            <> header "Calculate - A simple CLI calculator")

