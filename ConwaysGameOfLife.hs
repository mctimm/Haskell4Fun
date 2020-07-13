--Conways Game of Life in Haskell

data Cell = Alive | Dead deriving (Eq)

instance Show Cell where
    show Alive = "A"
    show Dead = "X"

data World = World [[Cell]] 

makeWorld x y = (World (take x (repeat (take y (repeat Dead)))))

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

headCons y (x:xs) = (y:x):(xs)
headCons y [] = [[y]]
taketurn :: World -> World
taketurn world = 
    let 
        helper world x y xCap yCap = if (y >= yCap)
            then []
            else if (x >= xCap)
                then []:helper world 0 (y+1) xCap yCap
                else (cellTurn (getCell world x y) world x y) `headCons` (helper world (x+1) (y) xCap yCap)
    in (World (helper world 0 0 (worldX world ) (worldY world)))