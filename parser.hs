import Text.ParserCombinators.Parsec

type Qualifier = String
type FunctionName = String
type Body = String

data Arguments = Maybe String | Nothing

data Function = Function Qualifier FunctionName Body
    deriving (Show)

parseFunction :: Parser Function 
parseFunction = do
        qualifier <- many1 (noneOf " ")
        char ' '
        string "function "
        name <- many1 (noneOf " \n{")
        parsePossibleWhitespace
        body <- parseBody
        return $ Function qualifier name body

parseWhitespace :: Parser String
parseWhitespace = many1 (oneOf " ")

parsePossibleWhitespace :: Parser String
parsePossibleWhitespace = many (oneOf " ")

parseArguments :: Parser Arguments 
parseArguments = Arguments Nothing

parseBody :: Parser String
parseBody = parseCurlyBlock

parseMultilineString :: Parser String
parseMultilineString = many (oneOf "\n abcdefghijklmnopqrstuvwxyz")

parseCurlyBlock :: Parser String
parseCurlyBlock = parseBlock '{' '}'

parseBlock :: Char -> Char -> Parser String
parseBlock x y = do
        char x
        content <- parseMultilineString
        char y
        return content

