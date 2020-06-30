evil :: Int->Bool
evil 1 = False
evil 0 = True
evil x = if (evil (x `mod` 2)) then (evil (x `div` 2)) else not (evil (x `div` 2))

e 1=1>2
e 0=1<2
e x= (i&&j)||not(i||j) 
    where i=e(mod x 2) 
          j=e(div x 2)

q x=0==(sum$map((+1).((((-2)*).(`mod`2))))x)

sqr x = x*x;
sumdigits x = foldr (\z y-> sqr((fromEnum z) - 48) + y) 0 (show x)
happyNumber x =
    let helper 1 _ = True
        helper x acc = if (sumdigits x) `elem` acc
            then False
            else helper (sumdigits x) ((sumdigits x):acc)
    in helper x []

f x=x:f(x/2) 

convertToNum x = if x >= '0' && x <= '9'
    then Just (fromEnum x - fromEnum '0')
    else Nothing

y \+\ Nothing = y
y \+\ Just x = y + x 

--function converts string to number
s [] = 0
s x = 
    let h x [] z = x + z
        h x (y:ys) z = if(convertToNum y) == Nothing
            then h (x+z) ys 0
            else h x ys ((10*z) \+\ (convertToNum y))
    in h 0 x 0 

(Just x) `justPlus` (Just y) = Just (x + y)
_ `justPlus` _ = Nothing 
[] \.\ [] = Just 0
(x:xs) \.\ (y:ys) = (Just (x*y) `justPlus` (xs \.\ ys)) 
_ \.\ _ = Nothing

succId = (id.succ)
