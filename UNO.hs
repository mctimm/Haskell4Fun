import Data.List
data Color = Red | Yellow | Blue | Green | Wild deriving (Eq, Enum, Show)
data Action = None | Reverse| Skip | Draw2 | Draw4 deriving (Enum, Show, Eq)
data Card = ActionCard Color Action | NumCard Color Int deriving (Eq)
wild = ActionCard Wild None
wild4 = ActionCard Wild Draw4

instance Show Card where
    show (ActionCard x None) = show x
    show (ActionCard x y) = (show x) ++ " " ++ (show y)
    show (NumCard x y) =  (show x) ++ " " ++ (show y)


createDeck = 
    let 
        helper :: Card -> [Card]
        helper (ActionCard Wild _) = take 8 (cycle [wild, wild4])
        helper (NumCard _ 10) = (take 32 (cycle [ActionCard x y| x<-[(Red)..(Green)], y<-[(Reverse)..(Draw2)]])) ++ (helper wild)
        helper (NumCard _ 0) =  ([NumCard x 0 | x<-[(Red)..(Green)]]) ++ (helper (NumCard (Green) 1))
        helper (NumCard _ _) = (take 72 (cycle [NumCard x y| x<-[(Red)..(Green)], y<-[1..9]])) ++ (helper (NumCard (Green) 10))
    in helper (NumCard (Green) 0)

canPlaceCard :: [Card] -> Card -> Bool
canPlaceCard _ (ActionCard Wild _ ) = True
canPlaceCard ((ActionCard Wild _):bs) _ = True
canPlaceCard ((ActionCard a b):bs) (ActionCard x y) = (a == x) || (b== y)
canPlaceCard ((NumCard a b):bs) (NumCard x y) = (a == x) || (b== y)
canPlaceCard ((ActionCard a b):bs) (NumCard x y) = (a == x)
canPlaceCard ((NumCard a b):bs) (ActionCard x y) = (a == x)

drawCard hand deck = (head deck:hand, tail deck)

draw2Cards hand deck =  (uncurry drawCard) (drawCard hand deck)

draw4Cards hand deck = (uncurry draw2Cards) (draw2Cards hand deck)

getAction (ActionCard _ y) = y

unAction (ActionCard y _) = ActionCard y None
unAction u@(NumCard y x) = u

comTurn hand deck pile = 
    let 
        helper hand deck pile [] = ((head deck):hand,tail deck,pile, unAction (head pile), False)
        helper hand deck pile (t:tempHand) = if canPlaceCard pile t
            then (delete t hand, deck, t:pile, t, True)
            else helper hand deck pile tempHand
    in helper hand deck pile hand

first5 (x,_,_,_,_) = x
second5 (_,x,_,_,_) = x
third5 (_,_,x,_,_) = x
fourth5 (_,_,_,x,_) = x
fifth5 (_,_,_,_,x) = x


unoTurn [] _ _ _ _ _ = True
unoTurn _ [] _ _ _ _ = False
unoTurn p1 p2 deck pile (ActionCard color Skip) turn = unoTurn p1 p2 deck pile (ActionCard color None) (not turn)
unoTurn p1 p2 deck pile (NumCard color number) False = unoTurn p1 (first5 k) (second5 k) (third5 k) (fourth5 k) (fifth5 k) where k = comTurn p2 deck pile