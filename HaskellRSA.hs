stringSizer x y= if length x >= y
    then x
    else stringSizer ("0" ++ x) y
convertString2Num x y= 
    let
        convertChars2Num [] _= []
        convertChars2Num (x:xs) y = ((\y x -> stringSizer x y) y (show(fromEnum x))):(convertChars2Num xs y)
    in convertChars2Num x y

convertNum2String [] _ = []
convertNum2String x y = (toEnum (read (take y x) :: Int) :: Char):(convertNum2String (drop y x) y)

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

encryptMessage m n e y= map (((\y x -> stringSizer x y)y).show) (map (encryptChunk n e) m)

totientFunction x y = lcm' (x-1) (y - 1)

decryptorStringSplit [] _= []
decryptorStringSplit xs y = (take y xs) :(decryptorStringSplit (drop y xs) y)

tuplemulti (x,y) z = (z*x,z*y)
tupleadd (x,y) (a,b)  = (x+a,y+b)

getDfromPrivatekey :: Int -> Int -> Int -> Maybe Int
getDfromPrivatekey x y e = 
    let totient = (totientFunction x y)
        helper :: (Int,Int) -> (Int,Int) -> Int -> Int -> Maybe Int
        helper tTuple eTuple t 0 = if t == 1 
            then Just (((snd tTuple) + totient) `mod` totient)
            else Nothing 
        helper tTuple eTuple t e = helper eTuple (tupleadd tTuple (tuplemulti eTuple ((0-1)*(div t e)))) e (mod t e)
    in helper (0,1) (1, ((-1)*(div totient e))) e (totient `mod` e) 

main = do
    putStrLn "encrypt,decrypt,or get d value? (e/d/g)"
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
            putStrLn (concat (encryptMessage (convertString2Num message (length keyString)) publicKey publicE (length keyString)))
        else if head text == 'd' 
            then do 
                putStrLn "Input your Message"
                message <- getLine
                putStrLn "Input your public key"
                keyString <- getLine
                putStrLn "Input your exponent"
                dString <- getLine
                let privateD = (read dString :: Int)
                    publicKey = (read keyString :: Int)
                putStrLn (convertNum2String (concat (encryptMessage (decryptorStringSplit message (length keyString)) publicKey privateD (length keyString))) (length keyString))
            else if head text == 'g'
                then do
                    putStrLn "Input your larger private key"
                    keyString1 <- getLine
                    putStrLn "Input your second private key"
                    keyString2 <- getLine
                    putStrLn "Input your exponent"
                    eString <- getLine
                    let privateKey1 = (read keyString1 :: Int)
                        privateKey2 = (read keyString2 :: Int)
                        e = (read eString :: Int)
                    putStrLn (show (getDfromPrivatekey privateKey1 privateKey2 e))
                else putStrLn "Invalid input"
