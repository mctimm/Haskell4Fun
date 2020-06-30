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

decryptorStringSplit [] = []
decryptorStringSplit xs = (take 3 xs) :(decryptorStringSplit (drop 3 xs))

tuplemulti (x,y) z = (z*x,z*y)
tupleadd (x,y) (a,b)  = (x+a,y+b)

getDfromPrivatekey :: Int -> Int -> Int -> Maybe (Int,Int)
getDfromPrivatekey x y e = 
    let helper :: (Int,Int) -> (Int,Int) -> Int -> Int -> Maybe (Int,Int)
        helper tTuple eTuple t 0 = if t == 1 
            then Just tTuple
            else Nothing 
        helper tTuple eTuple t e = helper eTuple (tupleadd tTuple (tuplemulti eTuple ((0-1)*(div t e)))) e (mod t e)
    in helper (0,1) (1, ((-1)*(div (totientFunction x y) e))) e ((totientFunction x y) `mod` e) 

main = do
    putStrLn "encrypt or decrypt? (e/d)"
    text <- getLine
    if head text == 'e'
        then do
            putStrLn "Input your Message"
            message <- getLine
            putStrLn "Input your public key"
            keyString <- getLine
            putStrLn "Input your exponent"
            eString <- getLine
            let publicE = (read eString :: Int)
                publicKey = (read keyString :: Int)
            putStrLn (concat (encryptMessage (convertString2Num message) publicKey publicE))
        else do 
            putStrLn "Input your Message"
            message <- getLine
            putStrLn "Input your public key"
            keyString <- getLine
            
            putStrLn "Input your exponent"
            dString <- getLine
            let privateD = (read dString :: Int)
                publicKey = (read keyString :: Int)
            putStrLn (convertNum2String (concat (encryptMessage (decryptorStringSplit message) publicKey privateD)))
