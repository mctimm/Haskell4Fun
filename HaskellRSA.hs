stringSizer x = if length x >= 3
    then x
    else stringSizer ("0" ++ x)
convertString2Num x = 
    let
        convertChars2Num [] = []
        convertChars2Num (x:xs) = (stringSizer(show(fromEnum x))):(convertChars2Num xs)
    in convertChars2Num x

convertNum2String [] = []
convertNum2String x = (toEnum (read (take 3 x) :: Int) :: Char):(convertNum2String (drop 3 x))

testPrime1 = 17
testPrime2 = 31

--both of these were just practice and are in the prelude.
gcd' x 1 = 1
gcd' x y = if k == 0
    then y
    else gcd y (x - (y*k))
    where k = x `mod` y

lcm' x y= (x*y) `div` (gcd' x y)

encryptChunk n e m = 
    let helper m x n 0 = x
        helper m x n e = helper m (mod (x*m) n) n (e-1)
    in helper (read m :: Int) 1 n e

encryptMessage m n e= map (stringSizer.show) (map (encryptChunk n e) m)

totientFunction x y = lcm' (x-1) (y - 1)



