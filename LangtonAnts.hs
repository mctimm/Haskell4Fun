--Langton Ants

data Direction = North | South | East | West deriving (Eq, Show)

data Ant = Ant Int Int Direction deriving (Show)

data Cell = Black | White deriving (Eq)

data World = World [[Cell]] Ant 

turnRight (World cells (Ant x y North)) = (World cells (Ant x y East))
turnRight (World cells (Ant x y South)) = (World cells (Ant x y West))
turnRight (World cells (Ant x y East)) = (World cells (Ant x y South))
turnRight (World cells (Ant x y West)) = (World cells (Ant x y North))

turnLeft (World cells (Ant x y North)) = (World cells (Ant x y West))
turnLeft (World cells (Ant x y South)) = (World cells (Ant x y East))
turnLeft (World cells (Ant x y East)) = (World cells (Ant x y North))
turnLeft (World cells (Ant x y West)) = (World cells (Ant x y South))

instance Show Cell where
    show (White) = "_"
    show (Black) = "#"

showLine xs x1 y1 = 
    let helper [] _ _ _ _ = []
        helper ([]:xs) x1 y1 x2 y2 = "\n" ++ helper xs x1 y1 0 (y2 + 1)
        helper ((x:xs):xss) x1 y1 x2 y2 = if (x2 == x1) && (y2 == y1)
            then "@" ++ (helper (xs:xss) x1 y1 (x2+1) y2)
            else show x ++ (helper (xs:xss) x1 y1 (x2+1) y2)
    in helper xs x1 y1 0 0

instance Show World where
    show (World cells (Ant x y _ )) = showLine cells x y

makeBasicWorld x y antX antY dir = World (take y (repeat (take x (repeat  (White))))) (Ant antX antY dir)

getCell :: World -> Int -> Int -> Cell
getCell (World z _) x y = if y >= length z || x >= length (head z) || x < 0 || y < 0
    then White
    else  (z !! y) !! x

replaceElem xs y index = 
    let helper (x:xs) y current index = if(current == index)
            then y:xs
            else x:(helper xs y (current+1) index)
    in helper xs y 0 index




changeCell :: World -> Int -> Int -> World
changeCell u@(World cells ant) x y = if (getCell u x y) == White
    then World (replaceElem cells (replaceElem (cells !! y) Black x) y) ant
    else World (replaceElem cells (replaceElem (cells !! y) White x) y) ant

moveAnt (World cells (Ant x y North)) = (World cells (Ant x (y - 1) North))
moveAnt (World cells (Ant x y South)) = (World cells (Ant x (y + 1) South))
moveAnt (World cells (Ant x y East)) = (World cells (Ant (x + 1) y East))
moveAnt (World cells (Ant x y West)) = (World cells (Ant (x - 1) y West))


takeTurn u@(World cells (Ant x y dir)) = if (getCell u x y) == White
    then (moveAnt.turnRight) (changeCell u x y)
    else (moveAnt.turnLeft) (changeCell u x y)

church f 0 y = id y
church f x y = church f (x - 1) (f y)

takeTurnRepeat u = do
    print u
    takeTurnRepeat (takeTurn u)


