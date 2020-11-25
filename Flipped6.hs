--Flipped6
import System.IO
import Control.Monad.Trans.Maybe
import Control.Monad.IO.Class
import System.Environment
import Control.Monad
import Control.Monad.Trans.State

kar :: Show a => [a] -> MaybeT IO a
kar [] = MaybeT (return Nothing) 
kar (x:xs) = do {liftIO (putStrLn (show x)); return x}

kdr :: Show a => [a] -> MaybeT IO [a]
kdr [] = MaybeT (return Nothing) 
kdr (x:xs) = do {liftIO (putStrLn (show xs)); return xs}

type Parser a = StateT String Maybe a

item :: Parser Char
item = StateT (\cs -> case cs of
                     ""     -> Nothing
                     (c:cs) -> Just (c,cs))

sat pred = do {c<-item; if pred c then return c else mzero}

string :: String -> Parser String
string [] = do {return []}
string (x:xs) = do {y<-sat (== x); ys<-string xs; return (y:ys)}