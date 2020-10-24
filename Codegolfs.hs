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


i y= and[x+foldr(\x y->(fromEnum x-48)+y)0(show x)/=y|x<-[0..y]]

--w [x] n = []
--w (x:xs) n = (x*n):(w xs n)

w x n=sum(init(map(*n) x)++(last x:[]))

permutations [] = []
permutations [x]  = [[x]] 
permutations (x:xs) = [(take i y) ++ (x:[]) ++ (drop i y)| y<-(permutations xs), i<-[0..length xs]]

--uniqueElems [] = 0
--uniqueElems [x] = 1
uniqueElems :: (Eq a) => [a]-> Int
uniqueElems =  (length.removeDuplicates)

removeDuplicates :: (Eq a) => [a] -> [a]
removeDuplicates = foldr (\x xs-> x:(filter (x/=) xs)) []

supermutationChecker xs = 
    let
        lists = (permutations.removeDuplicates) xs
        elems = uniqueElems xs
        helper xs n [] = True
        helper xs n (y:ys) = (any id [(take n (drop j xs)) == y| j<-[0..length xs]]) && helper xs n ys
    in helper xs elems lists

--solveRPN :: (Read a, Num a) => String -> a
--solveRPN xs = 
--    
--       
--       
--    in helper xs []

operators = ['+', '*', '-']

helper2 '+' = (+)
helper2 '-' = (-)
helper2 '*' = (*)
helper2 _  = mod

helper3 (x:xs) acc = if x == ' '
            then (acc,xs)
            else helper3 xs (acc*10 + (read $  x:[] ))

helper [] ys = head ys
helper u@(x:xs) ys = if x >= '0' && x <= '9'
    then helper (snd (helper3 xs (read $ x:[]))) (fst (helper3 xs (read $ x:[])):ys)
    else if x `elem` operators
        then helper xs (((helper2 x) (head ys)  (head (tail ys))):ys)
        else helper xs ys

isLetter x = (q >= fromEnum 'A' && q < fromEnum 'Z') || ( q >= fromEnum 'a' && q <= fromEnum 'z') where q = fromEnum x

modeSentence xs = 
    let 
        helper [] 0 ls = ls
        helper [] acc ls = acc:ls
        helper (x:xs) acc ls = if isLetter x
        then helper xs (succ acc) ls
        else if acc /= 0
            then helper xs 0 (acc:ls)
            else helper xs acc ls
    in mode (helper xs 0 [])

mode xs = 
    let listAdder x acc = if any id $ map (\y -> (x == ) (fst y)) acc
            then map (\y -> if (x== fst y) then (fst y,(snd y)+1) else y) acc 
            else ((x,1)):acc
        helper [] acc = acc
        helper (x:xs) acc = helper xs (listAdder x acc)
    in fst $ foldr (\x y-> if snd x > snd y then x else y) (head xs,0) (helper xs [])

k x=mod x 2<1 