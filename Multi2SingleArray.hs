--MultiDimensional Arrays as singles

data BasicArray = BasicArray [Double] [Int] deriving (Show)

--acc should start will a one. 
makeBasicArray :: [Int] -> [Int] -> BasicArray
makeBasicArray [x] acc = BasicArray [] (acc)
makeBasicArray (x:xs) acc = makeBasicArray xs ((x * (head acc) ):acc)


initBasicArray :: [Int] -> BasicArray -> BasicArray
initBasicArray inits (BasicArray [] acc) = BasicArray (take (product inits) (repeat 0.0)) acc

basicArray :: [Int] -> BasicArray
basicArray xs = initBasicArray xs (makeBasicArray (reverse xs) [1]) 

accessBasicArray (BasicArray vals indexes) xs =  vals !! (buildIndex xs indexes)

setBasicArray (BasicArray vals indexes) xs val = (BasicArray (insertValIntoList vals val (buildIndex xs indexes)) indexes)

insertValIntoList (v:vals) val 0 = val:vals
insertValIntoList (v:vals) val n = v:(insertValIntoList vals val (pred n))

buildIndex [x] [y] = x*y
buildIndex (x:xs) (y:ys) = x * y + buildIndex xs ys