module Main (get_maze, print_maze, is_wall, place_player, move, can_move, game_loop, get_path, main) where 

import System.Environment

maze_path = "overwrite this with your own path!"

-- Useful code from Lecture 25
-- You may use this freely in your solutions

get :: [String] -> Int -> Int -> Char
get maze x y = (maze !! y) !! x 

modify_list :: [a] -> Int -> a -> [a]
modify_list list pos new =
    let
        before = take  pos    list
        after  = drop (pos+1) list
    in
        before ++ [new] ++ after

set :: [String] -> Int -> Int -> Char -> [String]
set maze x y char = 
    let
        line = maze !! y
        new_line = modify_list line x char
        new_maze = modify_list maze y new_line
    in
        new_maze



---- Part A

-- Question 1

get_maze :: String -> IO [String]
get_maze maze = do
    x <- readFile maze
    let final = lines x
    return final



-- Question 2 

print_maze :: [String] -> IO ()
--print_maze = error "Not implemented"
print_maze maze = do
    let setup = unlines maze
    putStrLn setup


-- Question 3 

is_wall :: [String] -> (Int, Int) -> Bool
is_wall maze coordinates = do
    let x = fst coordinates
        y = snd coordinates 
        character = get maze x y
    if character == '#' then True else False

-- Question 4 

place_player :: [String] -> (Int, Int) -> [String]
place_player maze coordinates = set maze x y '@'
    where x = fst coordinates
          y = snd coordinates


---- Part B

-- Question 5 

move :: (Int, Int) -> Char -> (Int, Int)
move coordinates character
    | character == 'w' = (x,y-1)
    | character == 'a' = (x-1,y)
    | character == 's' = (x,y+1)
    | character == 'd' = (x+1,y)
    | otherwise = (x,y)
    where x = fst coordinates
          y = snd coordinates



-- Question 6 

can_move :: [String] -> (Int, Int) -> Char -> Bool
can_move maze coordinates character
    | mapChecker == True = False
    | mapChecker == False = True
    where mapChecker = is_wall maze mover
          mover = move coordinates character

-- Question 7 

game_loop :: [String] -> (Int, Int) -> IO ()
game_loop maze coordinates = do
    print_maze (place_player maze coordinates)
    input <- getLine
    let character = if input == "" then 'q' else head(input)
        checker = can_move maze coordinates character
        updatedCoordinates = if checker == True then move coordinates character else coordinates
        placingPlayerCorrect = place_player maze updatedCoordinates
    game_loop maze updatedCoordinates



---- Part C

-- Question 8 Helper functions

listOfLocations :: (Int,Int) -> [(Int,Int)]
listOfLocations currentLocation = [n] ++ [e] ++ [s] ++ [w]
    where n = move currentLocation 'w'
          e = move currentLocation 'd'
          s = move currentLocation 's'
          w = move currentLocation 'a'

perfectListOfLocations :: [(Int,Int)] -> [(Int,Int)] -> [(Int,Int)]
perfectListOfLocations _ [] = []
perfectListOfLocations visited (x:xs)
    | hasvisited == True = perfectListOfLocations visited xs
    | hasvisited == False = x : perfectListOfLocations visited xs
    where hasvisited = x `elem` visited

updatePath path list = map (\x -> addToPath path x) list

comparison currentPath [] = []
comparison currentPath (x:xs)
    | currentPath == x = comparison currentPath xs
    | currentPath /= x = x : comparison currentPath xs

finishedPath :: [[(Int,Int)]] -> (Int,Int) -> [(Int,Int)] -> [[(Int,Int)]]
finishedPath [] goal listOfCoordinates = []
finishedPath (x:xs) goal listOfCoordinates 
    | endOfPath == goal = [x]
    | endOfPath /= goal && listOfCoordinates == [] = xs
    | otherwise = (x:xs)
    where endOfPath = last x

addToPath path nextLocations = path ++ [nextLocations]

listOfPossibleLocations :: [(Int,Int)] -> [String] -> [(Int,Int)]
listOfPossibleLocations [] _ = []
listOfPossibleLocations (x:xs) maze
    | checksForWall == True = listOfPossibleLocations xs maze
    | checksForWall == False = x : listOfPossibleLocations xs maze 
    where checksForWall = is_wall maze x

-- Outputs all the paths necessary to achieve the right paths
depthFirstSearch start goal maze visited [] = []
depthFirstSearch start goal maze visited frontier = main ++ [currentPath]
    where locations = listOfLocations start
          possilbeLocations = listOfPossibleLocations locations maze
          ultimateLocations = perfectListOfLocations visited possilbeLocations
          currentPath = if (length frontier) == 0 then [(start)] else head (frontier)
          updateFrontier = updatePath currentPath ultimateLocations
          incompleteFrontier = updateFrontier ++ frontier
          filteredFrontier = comparison currentPath incompleteFrontier
          frontier1 = if last currentPath == goal then finishedPath filteredFrontier goal ultimateLocations else filteredFrontier
          visitedNew = start : visited
          currentLocation = last(head frontier1)
          main = depthFirstSearch currentLocation goal maze visitedNew frontier1 


-- Filters through the final list to get the correct path to the location
filterFinalList goal listOfPaths = filter (\x -> if goal == last(x) then True else False)listOfPaths


-- Question 8
get_path :: [String] -> (Int, Int) -> (Int, Int) -> [(Int, Int)]
--get_path = error "Not implemented"
get_path maze start end = path
    where searchingForPaths = depthFirstSearch start end maze [] [[start]]
          mainPath = filterFinalList end searchingForPaths
          path = head(mainPath)


-- Question 9 Helper functions
place_path :: [String] -> (Int, Int) -> [String]
place_path maze coordinate = set maze x y '.'
    where x = fst coordinate
          y = snd coordinate

place_pathes maze [] = []
place_pathes maze (x:xs) = newMaze : main
    where newMaze = place_path maze coordinate
          coordinate = x
          main = place_pathes newMaze xs

completedMaze maze coordinates = outputMaze
    where actualMaze = last (place_pathes maze coordinates)
          outputMaze = putStrLn (unlines actualMaze)

outputMaze :: [String] -> IO ()
outputMaze maze = do
    c <- return maze
    let x = (length (last c)) - 2
        y = (length c) - 2
        goal = (x,y) :: (Int,Int)
        path = get_path c (1,1) goal
        newMaze = place_pathes c path
        final = unlines (last newMaze)
    putStrLn final

-- Question 9
main :: IO ()
main = do
    x <- getLine
    let contents = readFile x
    boxContents <- contents
    let actualContents = lines boxContents
        answer = outputMaze actualContents
    boxAnswer <- answer
    return boxAnswer


















