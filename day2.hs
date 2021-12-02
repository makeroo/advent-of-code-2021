import System.IO
import Control.Monad


data Direction = Forward Integer | Up Integer | Down Integer deriving (Eq, Show)

data Position = Position Integer Integer deriving (Show)

-- distance, depth, aim
data Position3 = Position3 Integer Integer Integer

-- data AnyPos = Position | Position3

main = do
  contents <- readFile "day2.input"

  let commands = parseCommands (words contents)

  -- print commands

  let finalPos = move(Position 0 0, commands)
  let r = depth finalPos * distance finalPos

  print r

  let finalPos3 = move3(Position3 0 0 0, commands)

  let r = depth3 finalPos3 * distance3 finalPos3

  print r

move :: (Position, [Direction]) -> Position

move ( Position x y, Forward z:l ) = move(Position (x + z) y, l)
move ( Position x y, Up z:l )      = move(Position x (y - z), l)
move ( Position x y, Down z:l )    = move(Position x (y + z), l)
move ( p, [] )                     = p


move3 :: (Position3, [Direction]) -> Position3

move3 ( Position3 dist depth aim, Forward x:l ) = move3(Position3 (dist + x) (depth + aim * x) aim, l)
move3 ( Position3 dist depth aim, Down x:l ) = move3(Position3 dist depth (aim + x), l)
move3 ( Position3 dist depth aim, Up x:l ) = move3(Position3 dist depth (aim - x), l)
move3 ( p, [] ) = p


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
depth3 (Position3 _ z _) = z

-- distance :: AnyPos -> Integer
distance (Position d _) = d
distance3 (Position3 d _ _) = d
