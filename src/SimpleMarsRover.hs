module SimpleMarsRover where

data Direction = North | South | East | West deriving (Show)

data Position = Position {x :: Integer, y :: Integer} deriving (Show)

data Rover = Rover {position :: Position, direction :: Direction} deriving (Show)

data Command = LeftCmd | RightCmd | MoveCmd deriving (Show)

fromCharCommand :: String -> Command
fromCharCommand "L" = LeftCmd
fromCharCommand "R" = RightCmd
fromCharCommand "M" = MoveCmd
fromCharCommand _ = undefined

initial :: Rover
initial = Rover (Position 0 0) North

rotateLeft :: Rover -> Rover
rotateLeft (Rover p North) = Rover p West
rotateLeft (Rover p West) = Rover p South
rotateLeft (Rover p South) = Rover p East
rotateLeft (Rover p East) = Rover p South

rotateRight :: Rover -> Rover
rotateRight (Rover p North) = Rover p East
rotateRight (Rover p West) = Rover p North
rotateRight (Rover p South) = Rover p West
rotateRight (Rover p East) = Rover p South

moveNorth :: Position -> Position
moveNorth (Position x y) = undefined

execute :: String -> String
execute a = "2:3:N"