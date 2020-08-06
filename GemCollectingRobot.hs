data Robot = Robot Int Int [Int] deriving (Show,Eq)
data World = World [[Int]] Robot 

showLine xs x1 y1 = 
    let helper [] _ _ _ _ = []
        helper ([]:xs) x1 y1 x2 y2 = "\n" ++ helper xs x1 y1 0 (y2 + 1)
        helper ((x:xs):xss) x1 y1 x2 y2 = if (x2 == x1) && (y2 == y1)
            then "R" ++ (helper (xs:xss) x1 y1 (x2+1) y2)
            else show x ++ (helper (xs:xss) x1 y1 (x2+1) y2)
    in helper xs x1 y1 0 0

instance Show World where
    show (World cells (Robot x y _)) = showLine cells x y

moveRobot :: World -> World
moveRobot u@(World cells (Robot x y path)) = 
    let neighbors = (getCell u (x+1) y):(getCell u x (y+1)):(getCell u (x-1) y):(getCell u x (y-1)):[]
        movementsX = [1,0,-1,0]
        movementsY = [0,1,0,-1]
        helper cells (Robot x y paths) = if appearences (head paths) neighbors > 1
            then u
            else if appearences (head paths) neighbors == 0
                then helper cells (Robot x y (tail paths))
                else (World (replaceElem cells (replaceElem (cells !! y) 0 x) y) (Robot (x+ (movementsX !! unJust ( findElem (head paths) neighbors))) 
                    (y+ (movementsY !! unJust ( findElem (head paths) neighbors))) path))
    in helper cells (Robot x y path)

getCell :: World -> Int -> Int -> Int
getCell (World z _) x y = if y >= length z || x >= length (head z) || x < 0 || y < 0
    then 0
    else  (z !! y) !! x

replaceElem xs y index = 
    let helper (x:xs) y current index = if(current == index)
            then y:xs
            else x:(helper xs y (current+1) index)
    in helper xs y 0 index

trueToOne True = 1
trueToOne False = 0

appearences x [] = 0
appearences x ys = sum (map trueToOne [x == y| y<-ys])

findElem x xs = 
    let helper _ [] _ = Nothing
        helper x (y:ys) acc = if x == y
            then Just acc
            else helper x ys (acc+1)
    in helper x xs 0

unJust Nothing = -1
unJust (Just x) = x