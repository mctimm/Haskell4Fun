--Langton's Loop
import Data.Array
import Data.Ix
import Data.Char
import System.IO
import Data.List
import Control.Monad
import System.Console.ANSI

splitString :: Eq a => a -> [a] -> [[a]]
splitString elem string =
    let helper [] elem [] = []
        helper current elem [] = (current):[]
        helper current elem (x:xs) = if x == elem
            then current:(helper [] elem xs)
            else helper (current ++ (x:[])) elem xs
    in helper [] elem string

startLoop = ["022222222000000",
             "217014014200000",
             "202222220200000",
             "272000021200000",
             "212000021200000",
             "202000021200000",
             "272000021200000",
             "212222221222220",
             "207107107111112",
             "022222222222220"]

lulzToArray xs@(x:_) = array b [(i, (xs !! j) !! k) | i@(j,k) <- range b]
    where b = ((0,0),(m,n))
          n = length x - 1
          m = length xs - 1

arrayToLulz :: Array (Int,Int) Char -> [[Char]]
arrayToLulz a = [row a j | j <- rowIndices a]


printArray :: Array (Int,Int) Char -> IO ()
printArray a = sequence_ $ fmap putStrLn (arrayToLulz a)


digitToIntArray :: Ix i => Array i Char -> Array i Int
digitToIntArray = fmap digitToInt


intToDigitArray :: Ix i => Array i Int -> Array i Char
intToDigitArray = fmap (\x -> if x == 0 then ' ' else intToDigit x)

intToDigitArrayWhiteSpaces = fmap (\x -> if x == 0 then ' ' else intToDigit x)

colIndices a = [cMin..cMax] where ((_,cMin),(_,cMax)) = bounds a

row a j = [a ! (j,y) | y <- colIndices a]

rowIndices a = [rMin..rMax] where ((rMin,_),(rMax,_)) = bounds a

emptyArrayStates :: (Ix t, Num t) => e -> t -> Array t e
emptyArrayStates init m = array b [(i,init) | i <- range b]
    where b = (0,m)

emptyArray :: (Ix a, Ix b, Num a, Num b) => e -> a -> b -> Array (a, b) e
emptyArray init j k = array b [(i,init) | i <- range b]
    where b = ((0,0),(j,k))



rotateCodes :: Eq a => [a] -> [[a]]
rotateCodes xs = nub $ map (\x -> head xs :(x ++ (drop 5 xs))) q where q = rotate (tail (take 5 xs))

rotate (x:z:y:k:[]) = [[x,z,y,k],[k,x,z,y],[y,k,x,z],[z,y,k,x]]

makeTransitionsArray :: [[Char]] -> Array Int Int
makeTransitionsArray xs = (emptyArrayStates 8 77777) // [((read (init x) ::Int), digitToInt(last x)) | x<-xs]

a % (j,k) = a ! (j `mod` (m+1),k `mod` (n + 1)) where (m,n) = snd $ bounds a

inset a1 a2 = a1 // assocs a2


makeNumFromList :: Num t => [t] -> t
makeNumFromList xs = 
    let helper total [] = total
        helper total (x:xs) = helper ((total * 10) + x) xs
    in helper 0 xs

neighbors a (j,k) = [a %(j,k), a %(j-1,k), a % (j, k + 1), a %(j+1,k), a % (j, k - 1)]

tick :: Array (Int,Int) Int -> Array Int Int -> Array (Int,Int) Int
tick a states = array b [(i,states ! (makeNumFromList (neighbors a i))) | i <- indices a]
                where b = bounds a


lifeLoop a states times= do
    setCursorPosition 0 0
    printArray (intToDigitArrayWhiteSpaces a)
    lifeLoop (tick a states) states (times - 1)

main = do 
    clearScreen
    hideCursor
    handle <- openFile "langton-table.txt" ReadMode
    rawInput <- hGetContents handle
    let wordsList = splitString '\n' rawInput
    let statesArray = makeTransitionsArray $ concatMap rotateCodes wordsList
    lifeLoop (inset (emptyArray 0 100 100)  (digitToIntArray $ lulzToArray startLoop))  statesArray (1000)
    