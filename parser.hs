import Text.ParserCombinators.Parsec

-- can now parse a small function:
-- parse parseFunction "bla" "public function hi(args) {plus(3,4)\nmin(2,3)\nx=3\n}"

type Qualifier = String
type FunctionName = String
--type Body = String

data Body = Body [Statement]
    deriving (Show)

data Arguments = Arguments [String]
    deriving (Show)

data Statement = Assignment String String
               | Invocation String
    deriving (Show)

data Function = Function Qualifier FunctionName Arguments Body
    deriving (Show)

parseFunction :: Parser Function 
parseFunction = do
        qualifier <- many1 (noneOf " ")
        spaces
        string "function "
        name <- many1 (noneOf "(")
        arguments <- parseArguments
        spaces
        body <- parseBody
        return $ Function qualifier name arguments body

parseWhitespace :: Parser String
parseWhitespace = many1 (oneOf " ")

parsePossibleWhitespace :: Parser String
parsePossibleWhitespace = many (oneOf " ")

parseArguments :: Parser Arguments 
parseArguments = do
    char '('
    args <- many (noneOf ")")
    char ')'
    return $ Arguments [args]

parseBody :: Parser Body
parseBody = do
        char '{'
        statements <- many1 parseStatement
        char '}'
        return $ Body statements

parseTester :: Parser Body
parseTester = do 
        char '{'
        x <- many1 parseStatement
        return $ Body x

parseStatement :: Parser Statement
parseStatement = do
            x <- try parseInvocation <|> try parseAssignment <?> "statement"
            return x

parseInvocation :: Parser Statement
parseInvocation = do
        spaces
        name <- many1 (noneOf " (")
        spaces
        char '('
        spaces
        bla <- many1 (noneOf " )")
        spaces
        char ')'
        char '\n'
        return $ Invocation name

parseAssignment :: Parser Statement
parseAssignment = do
        spaces
        var <- many1 (noneOf " =")
        spaces
        char '=' <?> "equal in assignment"
        spaces
        value <- many1 (noneOf "\n")
        char '\n'
        spaces
        return $ Assignment var value



