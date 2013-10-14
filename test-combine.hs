import qualified Data.Map as M

type Name = String

data Event = DefRebound Name
           | OffRebound Name
           | MadeShot Name
           | MissedShot Name
           deriving (Show)

data Player = Player { madeShot :: Int, missedShot :: Int, defRebound :: Int, offRebound :: Int }
           deriving (Show)

defaultPlayer = Player{madeShot=0, missedShot=0, defRebound=0, offRebound=0}

type Players = M.Map Name Player

events = [DefRebound "PlayerA", OffRebound "PlayerB", MadeShot "PlayerC"]

playGame :: Players -> [Event] -> Players
playGame p [] = p
playGame p g = foldl updatePlayer p g
--playGame p (x:xs) = playGame (updatePlayer p x) xs

updatePlayer :: Players -> Event -> Players
updatePlayer p (DefRebound name) = M.insert name (addDefRebound (M.lookup name p)) p
updatePlayer p (OffRebound name) = M.insert name (addOffRebound (M.lookup name p)) p
updatePlayer p _                 = p

addDefRebound :: Maybe Player -> Player
addDefRebound (Just p) = Player (madeShot p) (missedShot p) ((defRebound p) + 1) (offRebound p)
addDefRebound (Nothing) = defaultPlayer{defRebound=1}

addOffRebound :: Maybe Player -> Player
addOffRebound (Just p) = Player (madeShot p) (missedShot p) (defRebound p) (offRebound p + 1)
addOffRebound (Nothing) = defaultPlayer{offRebound=1}

