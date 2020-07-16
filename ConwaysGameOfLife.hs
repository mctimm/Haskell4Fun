--Conways Game of Life in Haskell
import System.Random

data Cell = Alive | Dead deriving (Eq)

instance Show Cell where
    show Alive = "@"
    show Dead = "_"

data World = World [[Cell]] deriving (Eq)

makeDeadWorld x y = (World (take y (repeat (take x (repeat Dead)))))

headCons y (x:xs) = (y:x):(xs)
headCons y [] = [[y]]

makeRandomWorld :: Int -> Int -> Int -> World
makeRandomWorld x y seed =
    let 
        randomValues :: [Int]
        randomValues =  (randomRs (0,1) (mkStdGen seed))
        helper x y xCap yCap u@(r:rs) = if (y >= yCap)
            then []
            else if (x >= xCap)
                then []:(helper 0 (y+1) xCap yCap u)
                else if r == 1
                    then (Alive) `headCons` (helper (x + 1) y xCap yCap rs)
                    else (Dead) `headCons` (helper (x + 1) y xCap yCap rs)
    in World (helper 0 0 x y randomValues)


showLine [] = "\n"
showLine (x:xs) = show x ++ showLine xs

instance Show World where
    show (World x) = (concat (map showLine x))

getCell :: World -> Int -> Int -> Cell
getCell (World z) x y = if y >= length z || x >= length (head z) || x < 0 || y < 0
    then Dead
    else  (z !! y) !! x

getSurroundingCells world x y =
    let 
        helper _ _ _ _ 2 = []
        helper world x y 0 0 = helper world x y 1 0 
        helper world x y offsetX offsetY = if (abs(offsetX)) > 1
            then helper world x y (-1) (offsetY +1 )
            else (getCell world (x+offsetX) (y + offsetY)):(helper world x y (succ offsetX) offsetY)
    in helper world x y (-1) (-1)

testWorld = (World [[Alive,Dead,Dead],[Alive,Alive,Dead],[Alive,Alive,Dead]]) 
testWorld2 = (World [[Dead,Dead,Dead,Dead],[Dead,Alive,Alive,Dead],[Dead,Alive,Alive,Dead],[Dead,Dead,Dead,Dead]])
testWorld3 = (World [[Dead,Dead,Alive,Alive], [Dead,Alive,Alive,Dead],[Alive,Alive,Dead,Dead]])
cellTurn :: Cell -> World ->Int->Int -> Cell
cellTurn (Alive) world x y =  if neighbors == 2 || neighbors == 3 
    then Alive
    else Dead
    where neighbors = foldr (\cell y-> if cell == Alive then 1 + y else 0 + y) 0 (getSurroundingCells world x y)
cellTurn (Dead) world x y = if neighbors == 3 
    then Alive
    else Dead
    where neighbors = foldr (\cell y-> if cell == Alive then 1 + y else 0 + y) 0 (getSurroundingCells world x y)

worldX (World z) = (length (head z))
worldY (World z) = (length z) 

takeTurnReturn world = 
    let 
        helper world x y xCap yCap = if (y >= yCap)
            then []
            else if (x >= xCap)
                then []:helper world 0 (y+1) xCap yCap
                else (cellTurn (getCell world x y) world x y) `headCons` (helper world (x+1) (y) xCap yCap)
    in (world,(World (helper world 0 0 (worldX world ) (worldY world))))

takeTurn :: World -> World
takeTurn world = 
    let 
        helper world x y xCap yCap = if (y >= yCap)
            then []
            else if (x >= xCap)
                then []:helper world 0 (y+1) xCap yCap
                else (cellTurn (getCell world x y) world x y) `headCons` (helper world (x+1) (y) xCap yCap)
    in (World (helper world 0 0 (worldX world ) (worldY world)))

--not purely functional repeats infinitely 
takeTurnRepeat (oldWorld,nextWorld) = do
    print oldWorld
    if (oldWorld /= nextWorld)
        then takeTurnRepeat $ takeTurnReturn nextWorld
        else print nextWorld

