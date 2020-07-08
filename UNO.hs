
data Color = Red | Yellow | Blue | Green | Wild deriving (Eq, Enum, Show)
data Action = None | Reverse| Skip | Draw2 | Draw4 deriving (Enum, Show, Eq)
data Card = ActionCard Color Action | NumCard Color Int 
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

placeCard :: [Card] -> Card -> Bool
placeCard _ (ActionCard Wild _ ) = True
placeCard ((ActionCard a b):bs) (ActionCard x y) = (a == x) || (b== y)
placeCard ((NumCard a b):bs) (NumCard x y) = (a == x) || (b== y)
placeCard ((ActionCard a b):bs) (NumCard x y) = (a == x)
placeCard ((NumCard a b):bs) (ActionCard x y) = (a == x) 