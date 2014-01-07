import Text.ParserCombinators.Parsec
import qualified Data.Map as M
import Text.Parsec.Error (messageString, errorMessages)
import Data.Text (pack, unpack, strip)
import Data.Monoid

type TextLine = String
type Name = String

data ActionType = FoulCommit
                | GenericAction String
                | ActionType Player
    deriving (Show)

data Event = Time String
           | Action String ActionType
           | Unknown String
           | Error [String]
    deriving (Show)

data Player = Player { madeShot :: Int, missedShot :: Int, defRebound :: Int, offRebound :: Int , foulCommit :: Int, made3ptShot :: Int, missed3ptShot :: Int, steal :: Int, turnover :: Int, madeLayup :: Int, missedLayup :: Int, foulAgainst :: Int}
           deriving (Show)

type Players = M.Map Name Player

defaultPlayer = Player{madeShot=0, missedShot=0, defRebound=0, offRebound=0, foulCommit=0, made3ptShot=0, missed3ptShot=0, steal=0, turnover=0, madeLayup=0, missedLayup=0, foulAgainst=0}
defensiveReboundPlayer = defaultPlayer {defRebound=1}
offensiveReboundPlayer = defaultPlayer {offRebound=1}
madeShotPlayer = defaultPlayer {madeShot=1}
missedShotPlayer = defaultPlayer {missedShot=1}
foulCommitPlayer = defaultPlayer {foulCommit=1}
foulAgainstPlayer = defaultPlayer {foulAgainst=1}
made3ptShotPlayer = defaultPlayer {made3ptShot=1}
missed3ptShotPlayer = defaultPlayer {missed3ptShot=1}
stealPlayer = defaultPlayer {steal=1}
turnoverPlayer = defaultPlayer {turnover=1}

playerAppend :: Player -> Player -> Player
playerAppend (Player a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12) (Player b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11 b12) = Player (a1+b1) (a2+b2) (a3+b3) (a4+b4) (a5+b5) (a6+b6) (a7+b7) (a8 + b8) (a9+b9) (a10+b10) (a11+b11) (a12+b12)

instance Monoid Player where 
    mempty = defaultPlayer
    mappend = (playerAppend)

-- s <- readFile "/home/smu/code/fun-with-haskell/testfile.txt"
-- parse parseLines "bl" s


main = do
    s <- readFile "/home/smu/code/fun-with-haskell/testfile.txt"
    let res = parse parseLines "test" s
    print $ (filter (knownEvent) (extractResult res))

mainGame = do
    s <- readFile "/home/smu/code/fun-with-haskell/testfile.txt"
    let res = parse parseLines "test" s
    print $ playGame M.empty $ extractResult res

playGame :: Players -> [Event] -> Players
playGame p [] = p
playGame p g = foldl updatePlayer p g

updatePlayer :: Players -> Event -> Players
updatePlayer p (Action name (ActionType player)) = M.insertWith (mappend) name player p
updatePlayer p _                 = p

knownEvent :: Event -> Bool
knownEvent (Action _ (ActionType _)) = False
knownEvent _            = True

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
            action <- try parseMissed3pt <|> try parseFoul <|> try parseRebound <|> try parseSteal <|> try parseTurnover <|> parseGenericAction --try parseMissed3pt <|> try parseMade3pt <|> parseGenericAction
            return (Action (trimSpaces name) action)

parseFoul :: Parser ActionType
parseFoul = try parseFoulAgainst <|> parseFoulCommit

parseFoulCommit :: Parser ActionType
parseFoulCommit = do
            string "foul"
            s <- many (noneOf "\n")
            return $ ActionType foulCommitPlayer

parseSubstitution :: Parser String
parseSubstitution = do
            s <- many (oneOf "ABCDEFGHIJKLMNOPQRSTUVWXYZ -")
            s1 <- many (noneOf " ")
            string " replaces "
            s2 <- many (noneOf "\n")
            return s

parseFoulAgainst :: Parser ActionType
parseFoulAgainst = do
            string "foul against"
            s <- many (noneOf "\n")
            return $ ActionType foulAgainstPlayer

parseSteal :: Parser ActionType
parseSteal = do
            string "steal"
            s <- many (noneOf "\n")
            return $ ActionType stealPlayer

parseTurnover :: Parser ActionType
parseTurnover = do
            string "turnover"
            s<- many (noneOf "\n")
            return $ ActionType turnoverPlayer
            
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
            return $ ActionType defensiveReboundPlayer

parseOffRebound :: Parser ActionType
parseOffRebound = do
            string "offensive rebound"
            s <- many (noneOf "\n")
            return $ ActionType offensiveReboundPlayer 

parseMissed3pt :: Parser ActionType
parseMissed3pt = do
            string "missed 3-pointer"
            s <- many (noneOf "\n")
            return $ ActionType missed3ptShotPlayer

parseMade3pt :: Parser ActionType
parseMade3pt = do
            string "made 3-pointer"
            return $ ActionType made3ptShotPlayer

parseUnknown :: Parser Event
parseUnknown = do
            s <- many (noneOf "\n")
            return $ Unknown s

