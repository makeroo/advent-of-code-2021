import System.IO
import Control.Monad


data Direction = Forward Integer | Up Integer | Down Integer deriving (Eq, Show)

data Position = Position Integer Integer deriving (Show)


main = do
  contents <- readFile "day2.input"

  let commands = parseCommands (words contents)

  -- print commands

  let finalPos = move(Position 0 0, commands)
  let r = depth finalPos * distance finalPos

  print r

move :: (Position, [Direction]) -> Position

move ( Position x y, Forward z:l ) = move(Position (x + z) y, l)
move ( Position x y, Up z:l )      = move(Position x (y - z), l)
move ( Position x y, Down z:l )    = move(Position x (y + z), l)
move ( p, [] )                     = p

parseCommands :: [String] -> [Direction]

parseCommands ("forward":x:l) = Forward (toInt(x)) : parseCommands l
parseCommands ("down":x:l)    = Down (toInt(x)) : parseCommands l
parseCommands ("up":x:l)      = Up (toInt(x)) : parseCommands l
parseCommands []              = []
parseCommands _               = [] -- error "illegal input"

toInt :: String -> Integer
toInt = read

depth :: Position -> Integer
depth (Position _ z) = z

distance :: Position -> Integer
distance (Position d _) = d
