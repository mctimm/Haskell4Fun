import Data.Char

splitter _ [] = []
splitter f (x:xs) = if f x
    then []:splitter f xs
    else if null j
        then (x:[]):j
        else (x:head j):(tail j)
    where j = splitter f xs
elemfirst y= (any id).(map ((y==).(fst)))
addCounter :: (Eq a) => [(a,Int)]->a->[(a,Int)]
addCounter (u@(x,y):xs) z = if x == z
    then (x,(y+1)):xs
    else u:(addCounter xs z)
checkWord :: (Eq a) => [(a,Int)]->a->[(a,Int)]
checkWord xs y = if elemfirst y xs
    then addCounter xs y
    else (y,1):xs
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs = merge (mergeSort (drop lx xs)) (mergeSort (take lx xs)) where lx =  (length xs) `div` 2

merge [] ys = ys
merge xs [] = xs
merge u@(x:xs) j@(y:ys) = if snd x > snd y
    then x:(merge xs j)
    else y:(merge u ys)

fullCounter xs [] = xs
fullCounter xs (y:ys) = fullCounter (checkWord xs y) ys

main = do
    putStrLn "Input some text"
    text <- getContents
    putStrLn (show (mergeSort (filter (\x -> fst x /= "") (fullCounter [] (splitter (\x -> not (isLetter x)) (map toLower text))))))
