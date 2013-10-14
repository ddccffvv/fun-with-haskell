import Text.ParserCombinators.Parsec
import Text.Parsec.Error (messageString, errorMessages)
import Data.Text (pack, unpack, strip)

type TextLine = String

data ActionType = FoulCommit
                | GenericAction String
                | OffensiveRebound
                | DefensiveRebound
    deriving (Show)

data Event = Time String
           | Action String ActionType
           | Unknown String
           | Error [String]
    deriving (Show)


-- s <- readFile "/home/smu/code/fun-with-haskell/testfile.txt"
-- parse parseLines "bl" s


main = do
    s <- readFile "/home/smu/code/fun-with-haskell/testfile.txt"
    let res = parse parseLines "test" s
    print $ extractResult res


extractResult :: Either ParseError [Event] -> [Event]
extractResult (Right x) = x
extractResult (Left y) = [Error (map (messageString) (errorMessages y))]


trimSpaces :: String -> String
trimSpaces x = unpack $ strip $ pack x

parseLines :: Parser [Event]
parseLines = many parseLine

parseLine :: Parser Event
parseLine = do
            s <- try parseAction <|> try parseTime <|> parseUnknown
            newline
            return s

parseTime :: Parser Event
parseTime = do
            s <- many (noneOf ".")
            string ". min"
            return $ Time s

parseAction :: Parser Event
parseAction = do
            spaces
            name <- many (noneOf "-")
            char '-'
            spaces
            action <- try parseFoulCommit <|> try parseRebound <|> parseGenericAction
            return (Action (trimSpaces name) action)

parseFoulCommit :: Parser ActionType
parseFoulCommit = do
            string "foul"
            s <- many (noneOf "\n")
            return FoulCommit

parseGenericAction :: Parser ActionType
parseGenericAction = do
            s <- many (noneOf "\n")
            return $ GenericAction s

parseRebound :: Parser ActionType
parseRebound = do
            s <- try parseDefRebound <|> try parseOffRebound
            return s

parseDefRebound :: Parser ActionType
parseDefRebound = do
            string "defensive rebound"
            s <- many (noneOf "\n")
            return DefensiveRebound

parseOffRebound :: Parser ActionType
parseOffRebound = do
            string "offensive rebound"
            s <- many (noneOf "\n")
            return OffensiveRebound

parseUnknown :: Parser Event
parseUnknown = do
            s <- many (noneOf "\n")
            return $ Unknown s

