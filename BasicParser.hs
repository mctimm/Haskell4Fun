--Basic Parser in Haskell
import Parselib


data Statement = Let Expr Expr | Print Expr
-- data Expr = INT Int | RND Int | Var [Char] | ConstInt Int |Constr [Char] | Value Expr | PowerExpr Value PowerExpr
--             MultExpr PowerExpr MultExpr | NegateExpr MultExpr|  AddExpr NegateExpr AddExpr| ComExpr AddExpr ComExpr|  NotExpr ComExpr| AndExpr NotExpr AndExpr| OrExpr AndExpr OrExpr
--             | PrintList Expr PrintList | ExprList Expr ExprList 
data Expr = INT Expr | RND Expr | Var [Char] | ConstInt Int |Constr [Char] | Value Expr | PowerExpr Expr Expr [Char]
            | MultExpr Expr Expr [Char] | NegateExpr Expr |  AddExpr Expr Expr [Char] | ComExpr Expr Expr [Char]|  NotExpr Expr | AndExpr Expr Expr [Char] | OrExpr Expr Expr [Char]
            | PrintList Expr Expr [Char]| ExprList Expr Expr [Char] deriving (Show)
 

newLine = char '\n'

whitespace = space

driversLicense = do {c <- letter; cs <- many alphanum; return (c:cs)}

wordGrabber = do {char '"'; str<- many item; token $ char '"'; return str}

printList = chainerFinal1 [",",";"] expr PrintList

exprList = chainerFinal1 [","] expr ExprList





--GET THIS SHIT DONE: expr needs to defined, parses an expression
variable = do {c <-token driversLicense; return $ Var c}

-- letmaker = apply (do {int; token (string "LET"); v<-variable; token (char '='); x<-expr; Let v x})

value =  token (variable +++ function +++ constant +++ do {token $ char '('; e <- expr; token $ char ')'; return e})

constant :: Parser Expr
constant = do { x<-int; return $ ConstInt x} +++ do {str <- wordGrabber; return $ Constr str} 
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