--Basic Parser in Haskell
import Parselib
import Data.Array
import Data.Char
import System.IO
import Data.Bits
import Control.Monad.Trans.RWS.Lazy
import System.Random
import Control.Monad.IO.Class
import System.Environment
import Data.Ix
import Data.IORef

data Line = Line Int Statement deriving (Show)
data Statement = Statements Statement Statement [Char] | Let Expr Expr | Print Expr | END | Return |IF Expr Int | For Expr Expr Expr Expr| Next Expr Expr Expr Int | UNext Expr | NextBuddy Statement Statement [Char]
                    |Input [Char] Expr | Goto Int | Gosub Int | Rem Expr | Dim Expr | On Expr [Int] | InputBuddy Statement Statement [Char] deriving (Show) 
-- data Expr = INT Int | RND Int | Var [Char] | ConstInt Int |Constr [Char] | Value Expr | PowerExpr Value PowerExpr
--             MultExpr PowerExpr MultExpr | NegateExpr MultExpr|  AddExpr NegateExpr AddExpr| ComExpr AddExpr ComExpr|  NotExpr ComExpr| AndExpr NotExpr AndExpr| OrExpr AndExpr OrExpr
--             | PrintList Expr PrintList | ExprList Expr ExprList | SingleArray Variable ExprList| ArrayList SingleArray ArrayList | Null
data Expr = INT Expr | RND Expr | Var [Char] | ConstInt Double |Constr [Char] | Value Expr | PowerExpr Expr Expr [Char]
            | MultExpr Expr Expr [Char] | NegateExpr Expr |  AddExpr Expr Expr [Char] | ComExpr Expr Expr [Char]|  NotExpr Expr | AndExpr Expr Expr [Char] | OrExpr Expr Expr [Char]
            | PrintList Expr Expr [Char]| ExprList Expr Expr [Char] | SingleArray Expr Expr| ArrayList Expr Expr [Char] | Null | ConstArray BasicArray| ArrayAcess Expr Expr| Tab Expr deriving (Show)


newLine = char '\n'

whitespace = space

driversLicense = do {c <- letter; cs <- many alphanum; return (c:cs)}

wordGrabber = do {char '\"'; str<- many $ sat (/= '\"'); token $ char '\"'; return str}

statement = do {x<-int;s<-multiStatements;statementEnder;return (Line x s)}

multiStatements = do {kevin <-statements ; (kevinList, separator)<-chainerExpr ":" multiStatements; return $ Statements kevin kevinList separator} +++ statements 

statements = endStatement +++ letStatement  +++ forStatement +++ nextStatement +++ ifStatement +++ inputStatement +++ gotoStatement +++ printStatement +++ remStatement +++ dimStatement +++ gosubStatement +++ returnStatement +++ onStatement

forStatement =do {statementStarter "FOR"; i<-variable; token $ char '='; expr1 <- expr; token $ string "TO"; expr2 <- expr; token $ string "STEP" ; expr3 <- expr;  return $ (For i expr1 expr2 expr3)} +++  do {x<-statementStarter "FOR"; i<-variable; token $ char '='; expr1 <- expr; token $ string "TO"; expr2 <- expr; return $ (For i expr1 expr2 (ConstInt(1)))}

gosubStatement = do {statementStarter "GOSUB";  gotoNum<-int; return $ (Gosub gotoNum)}

returnStatement = do {statementStarter "RETURN"; return $ Return}

nextStatement = do {statementStarter "NEXT";i<-variable; nb<-nextBuddy; return $ NextBuddy (UNext i) (nb) ""} +++ do {statementStarter "NEXT"; i<-variable; return $ (UNext i)}

nextBuddy = do {token $ char ',';i<-variable; nb<-nextBuddy;return $ NextBuddy (UNext i) (nb) ""} +++ do {token $ char ',';i<-variable; return $ (UNext i)}

ifStatement = do {statementStarter "IF" ; exp<-expr; token $ string "THEN"; gotoNum<-int; return ((IF exp gotoNum))}

endStatement = do {statementStarter "END"; return $ END}

remStatement = do {statementStarter "REM"; str<-many $ sat (/= '$'); return ((Rem (Constr str)))}

dimStatement = do {statementStarter "DIM"; kevin<-arrayList; return ((Dim kevin))}

letStatement = do {statementStarter "LET"; v<-variable; token (char '('); y<-exprList; token (char ')'); token (char '='); x<-expr; return $ (Let (ArrayAcess v y) x)} +++ (do {statementStarter "LET"; v<-variable; token (char '='); x<-expr; return $ (Let v x) }) +++ do { v<-variable; token (char '('); y<-exprList; token (char ')'); token (char '='); x<-expr; return $ (Let (ArrayAcess v y) x)} +++ (do {v<-variable; token (char '='); x<-expr; return $ (Let v x) })

printStatement = do {statementStarter "PRINT"; printer <- token $ printList; return $  (Print printer)}

gotoStatement = do {statementStarter "GOTO"; gotoNum<-int; return $ (Goto gotoNum)}

onStatement = do {statementStarter "ON"; e <- expr; statementStarter "GOTO"; j<-int; i<-intBuddy; return $ On e (j:i)} +++  do {statementStarter "ON"; e <- expr; statementStarter "GOTO"; j<-int; return $ On e [j]}

intBuddy:: Parser [Int]
intBuddy = do {token $ char ','; i <-int;ib<-intBuddy;return (i:ib)} +++ do {token $ char ','; i <-int;return [i]}

inputStatement = do {statementStarter "INPUT";prompt<- wordGrabber;token $ char ';' ;v<-variable;ib<-inputBuddy;return $ InputBuddy (Input prompt v) (ib) ""} +++ do {statementStarter "INPUT"; v<-variable; ib<-inputBuddy; return $ InputBuddy (Input "" v) (ib) ""} +++ do {statementStarter "INPUT";prompt<- wordGrabber ;token $ char ';' ;v<-variable;return $ (Input prompt v)} +++ do {statementStarter "INPUT"; v<-variable; return $  (Input "" v)}

inputBuddy = do {token $ char ',';i<-variable; nb<-inputBuddy;return $ InputBuddy (Input "" i) (nb) ""} +++ do {token $ char ',';i<-variable; return $ (Input "" i)}

variable = do {c <-token driversLicense;token (char '('); y<-exprList; token (char ')'); return $ ArrayAcess (Var (c ++ "@")) y} +++  do {c <-token driversLicense; return $ Var c}

--this is the dumbest necessary function
arrayvar =  do {c <-token driversLicense; return $ Var (c ++ "@")}

printList = chainerFinalPrint [",",";"] expr PrintList

exprList = chainerFinal1 [","] expr ExprList

value =  token ( function +++ do {statementStarter "TAB";token $ char '('; e <- expr; token $ char ')';return $Tab e } +++ variable +++ constant +++  do {token $ char '('; e <- expr; token $ char ')';return $ Value e})

constant :: Parser Expr
constant = do { x<-int; return $ ConstInt (fromIntegral x)} +++ do {str <-token $ wordGrabber; return $ Constr str} 
--function needs some work
function = do {str <-token $ (string "INT" +++ string "RND"); token $ char '('; x<- expr; token $ char ')'; return $ if str == "INT" then INT x else RND x}

singleArray = do {name<-arrayvar; token $ char '('; el<-exprList; token $ char ')'; return $ SingleArray name el}

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

arrayList = do {kevin <-singleArray ; (kevinList, separator)<-chainerExpr "," arrayList; return $ ArrayList kevin kevinList separator} +++ singleArray 

chainerFinal1 :: [[Char]] -> Parser Expr -> (Expr -> Expr -> [Char]-> Expr) -> Parser Expr
chainerFinal1 lss childFunction exprType = token ( do {child <- token $ childFunction; (parent, op)<- token (chainerListExpr lss (chainerFinal1 lss childFunction exprType));(return (exprType child parent op))} +++ do {child <-  childFunction; (return child)} )
chainerFinal2 :: [Char] -> Parser Expr -> (Expr -> Expr) -> Parser Expr
chainerFinal2 str childFunction exprType = token ((do {token $ string str; child <- token $ childFunction; return $ exprType child}) +++ childFunction) 
chainerFinalPrint :: [[Char]] -> Parser Expr -> (Expr -> Expr -> [Char]-> Expr) -> Parser Expr
chainerFinalPrint lss childFunction exprType = token ( do {child <- token $ childFunction; (parent, op)<- token (chainerListExpr lss (chainerFinalPrint lss childFunction exprType));(return (exprType child parent op))} +++ do {child <-  childFunction; (return child)} ) +++ (do {space; return Null})



--chainerExpr :: [Char] -> Parser Expr -> Parser Expr 
chainerExpr c func =  token (do {token $ string c; x<-token func; return (x,c)})

--chainerListExpr :: [[Char]] -> Parser Expr -> Parser Expr
chainerListExpr [x] func = chainerExpr x func
chainerListExpr (x:xs) func = token ((chainerExpr x func ) +++ (chainerListExpr xs func))

statementStarter x = token $ string x

statementEnder = token (char '$')

--interpreter down below
getNumber (Line n _) = n
getStatement (Line _ s) = s
upperCaseFile ('\n':xs) = "  $\n" ++ (upperCaseFile xs)
upperCaseFile ('\"':xs) = '\"':(stringMiddle ++ ('\"':(upperCaseFile (tail ds)))) where {stringMiddle = takeWhile (/= '\"') xs; ds = dropWhile (/= '\"' ) xs}
upperCaseFile [] = " $"
upperCaseFile (x:xs) = (toUpper x:(upperCaseFile xs))

tupleListGetSnd n (x:xs) = if n == fst x 
    then (snd x) 
    else (tupleListGetSnd n xs)

-- statementRenumber ls (Line n (Statements statement1 statement2)) = 
-- statementRenumber ls (Line n (Gosub gotoNum)) = Line (tupleListGetSnd n ls) (Gosub (tupleListGetSnd gotoNum ls))
-- statementRenumber ls (Line n (IF x gotoNum)) = Line (tupleListGetSnd n ls) (IF x (tupleListGetSnd gotoNum ls)) 
-- statementRenumber ls (Line n (Goto gotoNum)) = Line (tupleListGetSnd n ls) (Goto (tupleListGetSnd gotoNum ls))
statementRenumber ls (Line n s)= Line (tupleListGetSnd n ls) (singleRenumber ls s)

singleRenumber ls (Statements statement1 statement2 sep) = (Statements (singleRenumber ls statement1) (singleRenumber ls statement2) sep)
singleRenumber ls (Gosub gotoNum) = (Gosub (tupleListGetSnd gotoNum ls))
singleRenumber ls (IF x gotoNum) = (IF x (tupleListGetSnd gotoNum ls))
singleRenumber ls (Goto gotoNum) = (Goto (tupleListGetSnd gotoNum ls))
singleRenumber ls (On e gotonums) = (On e (map (`tupleListGetSnd` ls) gotonums))
singleRenumber ls x = x

fileRenumber lsln = map (statementRenumber lsln)

forLinker _ _ _ [] = []
forLinker forLines forSteps forEnds (u@(Line n (For (Var variable) expr2 expr3 step)):lsStatements)  = u:(forLinker ((variable,n):forLines) ((variable,step):forSteps) ((variable,expr3):forEnds) lsStatements )
forLinker forLines forSteps forEnds ((Line n u@(UNext (Var variable))):lsStatements)  = ((Line n (nextMaker forLines forSteps forEnds u))):(forLinker forLines forSteps forEnds lsStatements )
forLinker forLines forSteps forEnds ((Line n u@(Statements statement1 statement2 c)):lsStatements) =
    let (v,(forLines2, forSteps2, forEnd2)) = forLinker2 forLines forSteps forEnds n u
    in (Line n v):(forLinker forLines2 forSteps2 forEnd2 lsStatements) 
forLinker forLines forSteps forEnds ((Line n u@(NextBuddy statement1 statement2 c)):lsStatements) =
    let (v,(forLines2, forSteps2, forEnd2)) = forLinker3 forLines forSteps forEnds n u
    in (Line n v):(forLinker forLines2 forSteps2 forEnd2 lsStatements)     
forLinker forLines forSteps forEnds (currentline:lsStatements) = currentline:(forLinker forLines forSteps forEnds lsStatements )

nextMaker forLines forSteps forEnds (UNext (Var variable)) = (Next (Var variable) (tupleListGetSnd variable forEnds) (tupleListGetSnd variable forSteps) (tupleListGetSnd variable forLines))


forLinker2 forLines forSteps forEnds n (Statements (UNext (Var variable)) statement2 c) = 
    let (u2,lss) = (forLinker2 (forLines) (forSteps) (forEnds) n statement2)
    in (Statements (Next (Var variable) (tupleListGetSnd variable forEnds) (tupleListGetSnd variable forSteps) (tupleListGetSnd variable forLines) ) u2 c, lss)
forLinker2 forLines forSteps forEnds n (Statements u@(For (Var variable) expr2 expr3 step) statement2 c) =
    let (u2,lss) = (forLinker2 ((variable,n):forLines) ((variable,step):forSteps) ((variable,expr3):forEnds) n statement2) 
    in(Statements u u2 c, lss) 
forLinker2 forLines forSteps forEnds n (Statements u statement2 c) =
    let (u2,lss) = (forLinker2 (forLines) (forSteps) (forEnds) n statement2)
    in (Statements u u2 c , lss) 
forLinker2 forLines forSteps forEnds n (UNext (Var variable)) = ((Next (Var variable) (tupleListGetSnd variable forEnds) (tupleListGetSnd variable forSteps) (tupleListGetSnd variable forLines) ), (forLines, forSteps, forEnds))
forLinker2 forLines forSteps forEnds n u@(For (Var variable) expr2 expr3 step) = (u,(((variable,n):forLines),((variable,step):forSteps),((variable,expr3):forEnds)))
forLinker2 forLines forSteps forEnds n s = (s,(forLines,forSteps, forEnds))

forLinker3 forLines forSteps forEnds n (NextBuddy (UNext (Var variable)) statement2 c) = 
    let (u2,lss) = (forLinker2 (forLines) (forSteps) (forEnds) n statement2)
    in (NextBuddy (Next (Var variable) (tupleListGetSnd variable forEnds) (tupleListGetSnd variable forSteps) (tupleListGetSnd variable forLines) ) u2 c, lss)
forLinker3 forLines forSteps forEnds n (NextBuddy u@(For (Var variable) expr2 expr3 step) statement2 c) =
    let (u2,lss) = (forLinker2 ((variable,n):forLines) ((variable,step):forSteps) ((variable,expr3):forEnds) n statement2) 
    in(NextBuddy u u2 c, lss) 
forLinker3 forLines forSteps forEnds n (NextBuddy u statement2 c) =
    let (u2,lss) = (forLinker2 (forLines) (forSteps) (forEnds) n statement2)
    in (NextBuddy u u2 c , lss) 
forLinker3 forLines forSteps forEnds n (UNext (Var variable)) = ((Next (Var variable) (tupleListGetSnd variable forEnds) (tupleListGetSnd variable forSteps) (tupleListGetSnd variable forLines) ), (forLines, forSteps, forEnds))
forLinker3 forLines forSteps forEnds n u@(For (Var variable) expr2 expr3 step) = (u,(((variable,n):forLines),((variable,step):forSteps),((variable,expr3):forEnds)))
forLinker3 forLines forSteps forEnds n s = (s,(forLines,forSteps, forEnds))

parseFile lineNumber lsln "$" stateLs = listArray (0,pred lineNumber) $ (forLinker [] [] []) $ reverse $ fileRenumber lsln stateLs
parseFile lineNumber lsln [] stateLs = listArray (0,pred lineNumber) $ (forLinker [] [] []) $ reverse $ fileRenumber lsln stateLs
parseFile lineNumber lsln input stateLs = 
    let k = head $ apply statement input
        newPair = (getNumber (fst k),lineNumber)
    in parseFile (succ lineNumber) (newPair:lsln) (snd k) ((fst k):stateLs) 

runSymbol :: String -> Double -> Double -> Double
runSymbol "<>" x y  = fromIntegral $ (fromEnum $ (x /= y)) 
runSymbol "<=" x y  = fromIntegral $ (fromEnum $ (x <= y)) 
runSymbol ">=" x y  = fromIntegral $ (fromEnum $ (x >= y)) 
runSymbol "="  x y  = fromIntegral $ (fromEnum $ (x == y)) 
runSymbol ">"  x y  = fromIntegral $ (fromEnum $ (x > y )) 
runSymbol "<"  x y  = fromIntegral $ (fromEnum $ (x < y )) 
runSymbol "+"  x y  = x + y 
runSymbol "-"  x y  = x - y 
runSymbol "*"  x y  = x * y 
runSymbol "/"  x y  = x / y 
runSymbol "^"  x y  = x ** y 
runSymbol "OR" x y  = fromIntegral $ ((.|.) (flooring x) (flooring y) ) 
runSymbol "AND" x y = fromIntegral $ ((.&.) (flooring x) (flooring y) )

flooring :: Double -> Int
flooring  = floor 

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
shouldFloor x = if (fromIntegral (floor x)) == x
    then show $ floor x
    else show $ x


runExpr ::  Expr ->  RWST (IORef [Int]) [Char] [([Char],Expr)] IO Double
runExpr (MultExpr expr1 expr2 symbol)  =  (runSymbol symbol) <$> (runExpr expr1) <*> (runExpr expr2)
runExpr (AddExpr expr1 expr2 symbol)   =  (runSymbol symbol) <$> (runExpr expr1) <*> (runExpr expr2)
runExpr (PowerExpr expr1 expr2 symbol) =  (runSymbol symbol) <$> (runExpr expr1) <*> (runExpr expr2)
runExpr (OrExpr expr1 expr2 symbol)    =  (runSymbol symbol) <$> (runExpr expr1) <*> (runExpr expr2)
runExpr (AndExpr expr1 expr2 symbol)   =  (runSymbol symbol) <$> (runExpr expr1) <*> (runExpr expr2)
runExpr (ComExpr expr1 expr2 symbol)   =  (runSymbol symbol) <$> (runExpr expr1) <*> (runExpr expr2)
runExpr (NegateExpr expr) =  ((-1) *) <$> (runExpr expr)
runExpr (NotExpr expr) =  (\x -> if (x == 0) then 1 else 0) <$> (runExpr expr)
runExpr (Value expr) =  runExpr expr
runExpr (ConstInt n) =  return n
runExpr (INT expr) =  do {exp<-runExpr expr; return $fromIntegral $  floor exp} --work on this
runExpr (RND expr) =  do { exp<-(runExpr expr);x<-(liftIO (randomRIO (0, exp))) ;if exp == 1 then return x else return  (fromIntegral $ pred $ flooring x)}
runExpr (Var name) = do {globalState <- get ;runExpr (tupleListGetSnd name globalState)}
runExpr (ArrayAcess (Var name) key) =  do {globalState <- get; key1<-(runExprList key);return $ getConstArray (tupleListGetSnd name globalState) (map floor key1)}
runExpr (ConstArray x) = return 0.0

runExprList (ExprList expr1 expr2 _) = do {e<-(runExpr expr1); e2 <- (runExprList expr2); return (e:e2)}
runExprList expr1 = do {e<-(runExpr expr1) ; return (e:[])}

runArrayList :: Expr ->  RWST (IORef [Int]) [Char] [([Char],Expr)] IO [([Char], Expr)]
runArrayList (SingleArray (Var name) exprList1) = do {e1<-runExprList exprList1; return ((name,ConstArray $ basicArray (map floor e1)):[])}
runArrayList (ArrayList expr1 expr2 _ ) = do {a<-(runArrayList expr1); a2 <- (runArrayList expr2); return (a ++ a2)}

runPrintSymbol " " x y = x ++ y
runPrintSymbol "," x y =  ((x ++ " ") ++ y) 
runPrintSymbol ";" x y = ((x ++ "\t") ++ y)
printRunExpr ::  Expr ->  RWST (IORef [Int]) [Char] [([Char],Expr)] IO String
printRunExpr (Null) = (return "$")
printRunExpr (PrintList expr1@(Constr string) expr2 ";") =  (runPrintSymbol " ") <$> (printRunExpr expr1) <*> (printRunExpr expr2)
printRunExpr (PrintList expr1 expr2 symbol) =  (runPrintSymbol symbol) <$> (printRunExpr expr1) <*> (printRunExpr expr2)
printRunExpr (Constr string) = return string
printRunExpr (Var x) = do{global<-get; printRunExpr (tupleListGetSnd x global)}
printRunExpr (ConstArray bs) = do {return $ show bs}
printRunExpr (Tab n) = do {n1 <- runExpr n; return $ take (floor n1) $  repeat ' '}
printRunExpr x = do { result<-(runExpr x);return $ shouldFloor result}

updateState [] x = [x]
updateState (u@(x1,y1):globalState) q@(x2,y2) = if x1 == x2
    then (x2,y2):globalState
    else u:(updateState globalState q)

removeState (u@(x1,y1):globalState) q@(x2,y2) = if x1 == x2
    then globalState
    else u:(removeState globalState q)

checkGlobalState str [] = False;
checkGlobalState str (x:xs) = (str == fst x) || (checkGlobalState str xs)

runStatements lineNumber (InputBuddy input1 ib _) = do {runStatement lineNumber input1; runStatements lineNumber ib; return $ Just (succ lineNumber)}
runStatements lineNumber (NextBuddy next nb _) = do {ln2 <-runStatement lineNumber next; if ln2 == Just (succ lineNumber) then runStatements lineNumber nb else return ln2}
runStatements lineNumber (Statements statement1 statement2 _) = do {statement1Number<-runStatement lineNumber statement1;statement2Number<-runStatements lineNumber statement2; return $ if statement1Number == (Just (succ lineNumber)) then statement2Number else statement1Number}
runStatements lineNumber x = runStatement lineNumber x

runStatement _ (END) = return Nothing
runStatement _ (Goto gotoNum) = return $ Just gotoNum
runStatement lineNumber (Gosub gotoNum) = do {kevin <- ask;kev <-liftIO $ readIORef kevin;liftIO (writeIORef kevin (lineNumber:kev));return $ Just gotoNum}
runStatement lineNumber (Return) = do {kevin <- ask;kev <-liftIO $ readIORef kevin;liftIO (writeIORef kevin (tail kev));return $ Just  $ succ $ head kev}
runStatement lineNumber (For (Var name) expr2 expr3 step) = do {exp<- (runExpr expr2);globalState<-get; if not (checkGlobalState name globalState) then (put (updateState globalState (name,ConstInt exp))) else put globalState; return $ (Just (succ lineNumber))}
runStatement lineNumber (Next (Var name)  expr3 step gotoNum) = do {globalState<-get;u<-(runExpr (AddExpr (tupleListGetSnd name globalState)  step "+")) ;s<-runExpr step;truthiness <-runExpr (ComExpr (ConstInt u) expr3 "<=");truthiness2 <-runExpr (ComExpr (ConstInt u) expr3 ">=");  put $  if s > 0 then (if truthiness /= 0 then (updateState globalState (name,ConstInt u)) else (removeState globalState (name,ConstInt u))) else (if truthiness2 /= 0 then (updateState globalState (name,ConstInt u)) else (removeState globalState (name,ConstInt u)));return $ if s > 0  then (if truthiness /= 0 then (Just gotoNum) else (Just (succ lineNumber))) else  (if truthiness2 /= 0 then (Just gotoNum) else (Just (succ lineNumber)))}
runStatement lineNumber (IF expr1 gotoNum) = do { truthiness <- runExpr expr1; return $ if truthiness /= 0 then (Just gotoNum) else (Just (succ lineNumber))}
runStatement lineNumber (Let (Var name) u@(Constr value)) = do {globalState<-get;put (updateState globalState (name,u)); return (Just (succ lineNumber))}
runStatement lineNumber (Let (Var name) value) = do {valueRan<-runExpr value; globalState<-get;put (updateState globalState (name,ConstInt valueRan)); return (Just (succ lineNumber))}
runStatement lineNumber (Let (ArrayAcess (Var name) access) value) = do {key <- runExprList access;valueRan<-runExpr value;updateArrayAccess name (map floor key) valueRan ; return (Just (succ lineNumber))}
runStatement lineNumber (Print Null) = do {liftIO $ putStr "\n"; return (Just (succ lineNumber))}
runStatement lineNumber (Print expr1) = do {printable<-(printRunExpr expr1); if (last printable == '$') then (liftIO (putStr (init printable))) else (liftIO (putStrLn printable)); return (Just (succ lineNumber))}
runStatement lineNumber (Input prompt (Var name)) = do {(liftIO $putStr (prompt ++ "\t" ));liftIO (hFlush stdout) ;input<-(liftIO getLine); globalState<-get;put (updateState globalState (name, takeInput input )); return (Just (succ lineNumber))}
runStatement lineNumber (Rem _) = return (Just (succ lineNumber))
runStatement lineNumber (Dim expr) = do {listOfArrays<-runArrayList expr; globalState<-get;put (listOfArrays ++ globalState); return  (Just (succ lineNumber))}
runStatement lineNumber (On e gotonums) = do {e1<-runExpr e ;if (floor e1) <= length gotonums && (floor e1) > 0 then return $ Just (gotonums !! ((floor e1) - 1)) else return $ Just (succ lineNumber)}


updateArrayAccess :: [Char] -> [Int] -> Double -> RWST (IORef [Int]) [Char] [([Char],Expr)] IO String 
updateArrayAccess name key val = do {globalState <- get; put (updateState globalState (name,setConstArray (tupleListGetSnd name globalState) key val)); return "0"}

setConstArray (ConstArray a1) key val = ConstArray (setBasicArray a1 key val)
getConstArray (ConstArray a1) key = (accessBasicArray a1 key)

takeInput :: [Char] -> Expr
takeInput x = if all id $ (map (\y -> isNumber y|| y == '.' || y == '-') x)
    then ConstInt (read x :: Double)
    else Constr x

unjust Nothing = -1
unjust (Just x) = x

mainLoop :: Array Int Line -> Int ->  RWST (IORef [Int]) [Char] [([Char],Expr)] IO (Maybe Int) 
mainLoop input lineNumber = do
    x <- runStatements lineNumber (getStatement (input ! lineNumber))
    if x /= Nothing then mainLoop input (unjust x) else return x

main = do
    args <- getArgs
    handle <- openFile (head args) ReadMode
    rawInput <- hGetContents handle
    putStrLn $ upperCaseFile rawInput 
    let input = parseFile 0 [] (upperCaseFile rawInput) []
    print input
    kevin <-(newIORef [])
    runRWST (mainLoop input 0) kevin []

    --MultiDimensional Arrays as singles



data BasicArray = BasicArray [Double] [Int] deriving (Show)

--acc should start will a one. 
makeBasicArray :: [Int] -> [Int] -> BasicArray
makeBasicArray [x] acc = BasicArray [] (acc)
makeBasicArray (x:xs) acc = makeBasicArray xs ((x * (head acc) ):acc)


initBasicArray :: [Int] -> BasicArray -> BasicArray
initBasicArray inits (BasicArray [] acc) = BasicArray (take  (product (map succ inits)) (repeat 0.0)) acc

basicArray :: [Int] -> BasicArray
basicArray xs = initBasicArray xs (makeBasicArray (reverse xs) [1]) 

accessBasicArray (BasicArray vals indexes) xs =  vals !! (buildIndex xs indexes)

setBasicArray (BasicArray vals indexes) xs val = (BasicArray (insertValIntoList vals val (buildIndex xs indexes)) indexes)

insertValIntoList [] val _ = []
insertValIntoList (v:vals) val 0 = val:vals
insertValIntoList (v:vals) val n = v:(insertValIntoList vals val (pred n))

buildIndex [x] [y] = x*y
buildIndex (x:xs) (y:ys) = x * y + buildIndex xs ys