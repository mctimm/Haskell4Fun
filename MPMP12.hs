listOfDivisors :: Integral a => a -> [a]
listOfDivisors x = [y | y <- [1..x], x `mod` y == 0]
marchingBandFinder :: Integral a => Int -> a
marchingBandFinder x = last $ head [z | z<- (map listOfDivisors [1..2^x]), length z == x]

main = do
    putStrLn "Enter a number of formations"
    x <- getLine
    putStrLn $ "You would need at least " ++ show (marchingBandFinder (read x :: Int)) ++ " people"