convertString2Num x = 
    let stringSizer x = if length x >= 3
        then x
        else stringSizer ("0" ++ x)
        convertChars2Num [] = []
        convertChars2Num (x:xs) = (stringSizer(show(fromEnum x))):(convertChars2Num xs)
    in concat (convertChars2Num x)

convertNum2String [] = []
convertNum2String x = (toEnum (read (take 3 x) :: Int) :: Char):(convertNum2String (drop 3 x))

testPrime1 = 17
testPrime2 = 31

gcd' x 1 = 1
gcd' x y = if k == 0
    then y
    else gcd y (x - (y*k))
    where k = x `mod` y

lcm' x y= (x*y) `div` (gcd' x y)  

