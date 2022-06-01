module SimpleMarsRover where

data Direction = North | South | East | West deriving (Show)

data Position = Position {x :: Integer, y :: Integer} deriving (Show)

data Rover = Rover {position :: Position, direction :: Direction} deriving (Show)

data Command = LeftCmd | RightCmd | MoveCmd deriving (Show)

initial :: Rover
initial = Rover (Position 0 0) North

rotateLeft :: Rover -> Rover
rotateLeft (Rover p North) = Rover p West
rotateLeft (Rover p West) = Rover p South
rotateLeft (Rover p South) = Rover p East
rotateLeft (Rover p East) = Rover p North

rotateRight :: Rover -> Rover
rotateRight (Rover p North) = Rover p East
rotateRight (Rover p West) = Rover p North
rotateRight (Rover p South) = Rover p West
rotateRight (Rover p East) = Rover p South

increase :: Integer -> Integer
increase 9 = 0
increase n = n + 1

decrease :: Integer -> Integer
decrease 0 = 9
decrease n = n - 1

moveNorth :: Position -> Position
moveNorth (Position x y) = Position x (increase y)

moveSouth :: Position -> Position
moveSouth (Position x y) = Position x (decrease y)

moveEast :: Position -> Position
moveEast (Position x y) = Position (increase x) y

moveWest :: Position -> Position
moveWest (Position x y) = Position (decrease x) y

move :: Rover -> Rover
move (Rover p North) = Rover (moveNorth p) North
move (Rover p South) = Rover (moveSouth p) South
move (Rover p East) = Rover (moveEast p) East
move (Rover p West) = Rover (moveWest p) West

executeCommand :: Rover -> Command -> Rover
executeCommand r LeftCmd = rotateLeft r
executeCommand r RightCmd = rotateRight r
executeCommand r MoveCmd = move r

executeCommands :: Rover -> [Command] -> Rover
executeCommands = foldl executeCommand

commandsFromInput :: [Char] -> [Command]
commandsFromInput = map fromCharCommand

fromCharCommand :: Char -> Command
fromCharCommand 'L' = LeftCmd
fromCharCommand 'R' = RightCmd
fromCharCommand 'M' = MoveCmd
fromCharCommand _ = undefined

outputFromDirection :: Direction -> String
outputFromDirection North = "N"
outputFromDirection South = "S"
outputFromDirection East = "E"
outputFromDirection West = "W"

outputFromRover :: Rover -> String
outputFromRover (Rover (Position x y) d) = show x <> ":" <> show y <> ":" <> outputFromDirection d

execute :: String -> String
execute s = outputFromRover $ executeCommands initial $ commandsFromInput s