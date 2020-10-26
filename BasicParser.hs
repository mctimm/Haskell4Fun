--Basic Parser in Haskell
import Parselib
import Data.Array
import System.IO
import Data.Bits
import Control.Monad.Trans.State.Lazy
import System.Random
import Control.Monad.IO.Class

data Line = Line Int Statement deriving (Show, Eq)
data Statement = Let Expr Expr | Print Expr | END deriving (Show, Eq)
-- data Expr = INT Int | RND Int | Var [Char] | ConstInt Int |Constr [Char] | Value Expr | PowerExpr Value PowerExpr
--             MultExpr PowerExpr MultExpr | NegateExpr MultExpr|  AddExpr NegateExpr AddExpr| ComExpr AddExpr ComExpr|  NotExpr ComExpr| AndExpr NotExpr AndExpr| OrExpr AndExpr OrExpr
--             | PrintList Expr PrintList | ExprList Expr ExprList 
data Expr = INT Expr | RND Expr | Var [Char] | ConstInt Int |Constr [Char] | Value Expr | PowerExpr Expr Expr [Char]
            | MultExpr Expr Expr [Char] | NegateExpr Expr |  AddExpr Expr Expr [Char] | ComExpr Expr Expr [Char]|  NotExpr Expr | AndExpr Expr Expr [Char] | OrExpr Expr Expr [Char]
            | PrintList Expr Expr [Char]| ExprList Expr Expr [Char] deriving (Show, Eq)
 

newLine = char '\n'

whitespace = space

driversLicense = do {c <- letter; cs <- many alphanum; return (c:cs)}

wordGrabber = do {char '\"'; str<- many $ sat (/= '\"'); token $ char '\"'; return str}

statement = endStatement +++ letStatement +++ printStatement



endStatement = do {x<-statementStarter "END"; return $ Line x END}


letStatement = (do {i<-statementStarter "LET"; v<-variable; token (char '='); x<-expr; return $ Line i (Let v x) })


printStatement = do {x<-statementStarter "PRINT"; printer <- printList; return $ Line x (Print printer)}




variable = do {c <-token driversLicense; return $ Var c}

printList = chainerFinal1 [",",";"] expr PrintList

exprList = chainerFinal1 [","] expr ExprList

value =  token ( function +++ variable +++ constant +++ do {token $ char '('; e <- expr; token $ char ')';return $ Value e})

constant :: Parser Expr
constant = do { x<-int; return $ ConstInt x} +++ do {str <-token $ wordGrabber; return $ Constr str} 
--function needs some work
function = do {str <-token $ (string "INT" +++ string "RND"); token $ char '('; x<- expr; token $ char ')'; return $ if str == "INT" then INT x else RND x}

-- powerExpr = do {val1 <- value; return $ val1 +++ PowerExp val1 (chainerExpr "^" powerExpr)}
powerExpr = chainerFinal1 ["^"] value PowerExpr

-- multExpr =  do {negate1 <- negateExpr; return $ negate1 +++ MultExpr negate1 (chainerListExpr ["*","/"] multExpr}
multExpr = chainerFinal1 ["*","/"] powerExpr MultExpr

-- negateExpr = powerExpr +++ (do {char '-'; pow <- powerExpr; return $ NegateExp pow})
negateExpr = chainerFinal2 "-" multExpr NegateExpr

-- we forgot to make this the first time, but our form makes it easy!
addExpr = chainerFinal1 ["+","-"] negateExpr AddExpr

-- comExpr =  do {mult1 <- multExpr; return $ mult1 +++ ComExpr mult1 (chainerListExpr ["=","<>", ">", ">=", "<", "<="] comExpr}
comExpr = chainerFinal1 ["=","<>", ">", ">=", "<", "<="] addExpr ComExpr

-- notExpr = comExpr +++ do {string "NOT"; com1 <- comExpr; return $ NotExpr comExpr}
notExpr = chainerFinal2 "NOT" comExpr NotExpr

-- andExpr =  do {not1 <- negateExpr; return $ not1 +++ AndExpr not1 (chainerListExpr ["AND"] andExpr}
andExpr = chainerFinal1 ["AND"] notExpr AndExpr

-- hadn't done this one yet either.
orExpr = chainerFinal1 ["OR"] andExpr OrExpr
-- we wanted it to be called orExpr because that naturally follows...
-- but don't want it to be known as orExpr throughout the rest of the program.
expr = token orExpr



chainerFinal1 :: [[Char]] -> Parser Expr -> (Expr -> Expr -> [Char]-> Expr) -> Parser Expr
chainerFinal1 lss childFunction exprType = token ( do {child <- token $ childFunction; (parent, op)<- token (chainerListExpr lss (chainerFinal1 lss childFunction exprType));(return (exprType child parent op))} +++ do {child <-  childFunction; (return child)} )
chainerFinal2 :: [Char] -> Parser Expr -> (Expr -> Expr) -> Parser Expr
chainerFinal2 str childFunction exprType = token ((do {token $ string str; child <- token $ childFunction; return $ exprType child}) +++ childFunction) 

--chainerExpr :: [Char] -> Parser Expr -> Parser Expr 
chainerExpr c func =  token (do {token $ string c; x<-token func; return (x,c)})

--chainerListExpr :: [[Char]] -> Parser Expr -> Parser Expr
chainerListExpr [x] func = chainerExpr x func
chainerListExpr (x:xs) func = token ((chainerExpr x func ) +++ (chainerListExpr xs func))

statementStarter x = do {lineNumber<-int; token $ string x; return lineNumber}


--interpreter down below
getNumber (Line n _) = n
getStatement (Line _ s) = s

tupleListGetSnd n (x:xs) = if n == fst x 
    then (snd x) 
    else (tupleListGetSnd n xs)

statementRenumber ls (Line n s)= Line (tupleListGetSnd n ls) s

fileRenumber lsln = map (statementRenumber lsln)


parseFile lineNumber lsln [] stateLs = listArray (0,pred lineNumber) $ reverse $ fileRenumber lsln stateLs
parseFile lineNumber lsln input stateLs = 
    let k = head $ apply statement input
        newPair = (getNumber (fst k),lineNumber)
    in parseFile (succ lineNumber) (newPair:lsln) (snd k) ((fst k):stateLs) 

runSymbol "<>" x y  = fromIntegral $ (fromEnum $ (x /= y)) 
runSymbol "<=" x y  = fromIntegral $ (fromEnum $ (x <= y)) 
runSymbol ">=" x y  = fromIntegral $ (fromEnum $ (x >= y)) 
runSymbol "="  x y  = fromIntegral $ (fromEnum $ (x == y)) 
runSymbol ">"  x y  = fromIntegral $ (fromEnum $ (x > y )) 
runSymbol "<"  x y  = fromIntegral $ (fromEnum $ (x < y )) 
runSymbol "+"  x y  = x + y 
runSymbol "-"  x y  = x - y 
runSymbol "*"  x y  = x * y 
runSymbol "/"  x y  = x `div` y 
runSymbol "^"  x y  = x ^ y 
runSymbol "OR" x y  = (.|.) x y  
runSymbol "AND" x y = (.&.)  x y 

--not State stuff
--runExpr :: (Num a) => Expr -> a
-- runExpr (MultExpr expr1 expr2 symbol)  =  (runSymbol symbol) (runExpr expr1)  (runExpr expr2)
-- runExpr (AddExpr expr1 expr2 symbol)   =  (runSymbol symbol) (runExpr expr1)  (runExpr expr2)
-- runExpr (PowerExpr expr1 expr2 symbol) =  (runSymbol symbol) (runExpr expr1)  (runExpr expr2)
-- runExpr (OrExpr expr1 expr2 symbol)    =  (runSymbol symbol) (runExpr expr1)  (runExpr expr2)
-- runExpr (AndExpr expr1 expr2 symbol)   =  (runSymbol symbol) (runExpr expr1)  (runExpr expr2)
-- runExpr (ComExpr expr1 expr2 symbol)   =  (runSymbol symbol) (runExpr expr1)  (runExpr expr2)
-- runExpr (NegateExpr expr) =  (-1) * (runExpr expr)
-- runExpr (NotExpr expr) =  if (runExpr expr) == 0
--     then 1
--     else 0
-- runExpr (Value expr) =  runExpr expr
-- runExpr (ConstInt n) =  fromIntegral n
-- runExpr (INT expr) =  runExpr expr --work on this
-- runExpr (RND expr) =  runExpr expr
--runExpr (Var name) = do {globalState<-get; return $ tupleListGetSnd name globalState}

runExpr ::  Expr -> StateT [([Char],Expr)] IO Int
runExpr (MultExpr expr1 expr2 symbol)  =  (runSymbol symbol) <$> (runExpr expr1) <*> (runExpr expr2)
runExpr (AddExpr expr1 expr2 symbol)   =  (runSymbol symbol) <$> (runExpr expr1) <*> (runExpr expr2)
runExpr (PowerExpr expr1 expr2 symbol) =  (runSymbol symbol) <$> (runExpr expr1) <*> (runExpr expr2)
runExpr (OrExpr expr1 expr2 symbol)    =  (runSymbol symbol) <$> (runExpr expr1) <*> (runExpr expr2)
runExpr (AndExpr expr1 expr2 symbol)   =  (runSymbol symbol) <$> (runExpr expr1) <*> (runExpr expr2)
runExpr (ComExpr expr1 expr2 symbol)   =  (runSymbol symbol) <$> (runExpr expr1) <*> (runExpr expr2)
runExpr (NegateExpr expr) =  ((-1) *) <$> (runExpr expr)
runExpr (NotExpr expr) =  (\x -> if (x == 0) then 1 else 0) <$> (runExpr expr)
runExpr (Value expr) =  runExpr expr
runExpr (ConstInt n) =  return $ n
runExpr (INT expr) =  runExpr expr --work on this
runExpr (RND expr) =  do { exp<-(runExpr expr);x<- (liftIO (randomRIO (0, exp - 1))) ; return x}
runExpr (Var name) = do {globalState <- get ;runExpr (tupleListGetSnd name globalState)}

runPrintSymbol "," x y =  ((x ++ " ") ++ y) 
runPrintSymbol ";" x y = ((x ++ "\t") ++ y)
printRunExpr ::  Expr -> StateT [([Char],Expr)] IO String
printRunExpr (PrintList expr1 expr2 symbol) =  (runPrintSymbol symbol) <$> (printRunExpr expr1) <*> (printRunExpr expr2)
printRunExpr (Constr string) = return string
printRunExpr x = do { result<-(runExpr x);return $ show result}

updateState [] x = [x]
updateState (u@(x1,y1):globalState) q@(x2,y2) = if x1 == x2
    then (x2,y2):globalState
    else u:(updateState globalState q)

--this is the current working area
runStatement _ (END) = return Nothing
runStatement lineNumber (Let (Var name) value) = do {valueRan<-runExpr value; globalState<-get;put (updateState globalState (name,ConstInt valueRan)); return (Just (succ lineNumber))}
runStatement lineNumber (Print expr1) = do {printable<-(printRunExpr expr1); (liftIO (putStr printable)); return (Just (succ lineNumber))}

unjust Nothing = -1
unjust (Just x) = x

mainLoop :: Array Int Line -> Int -> StateT [([Char], Expr)] IO (Maybe Int) 
mainLoop input lineNumber = do
    x <- runStatement lineNumber (getStatement (input ! lineNumber))
    if x /= Nothing then mainLoop input (unjust x) else return x

main = do 
    handle <- openFile "BasicFiles\\foo.bas" ReadMode
    rawInput <- hGetContents handle
    let input = parseFile 0 [] rawInput []
    runStateT (mainLoop input 0) []