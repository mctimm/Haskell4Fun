--Matt Parker's Matt's puzzles 10

data Board = Board ((Bool,Bool,Bool,Bool,Bool),(Bool,Bool,Bool,Bool,Bool),(Bool,Bool,Bool,Bool,Bool),(Bool,Bool,Bool,Bool,Bool),(Bool,Bool,Bool,Bool,Bool))

instance Show Board where
    show (Board z) = show (select51 z) ++ "\n"  ++ show (select52 z)++ "\n" ++ show (select53 z)++ "\n" ++ show (select54 z) ++ "\n" ++ show (select55 z) 

select51 (x,_,_,_,_) = x
select52 (_,x,_,_,_) = x
select53 (_,_,x,_,_) = x
select54 (_,_,_,x,_) = x
select55 (_,_,_,_,x) = x

selector5 1 = select51
selector5 2 = select52
selector5 3 = select53
selector5 4 = select54
selector5 5 = select55

selector5x5 x y (Board z) = (selector5 x(selector5 y z))

boardChecker z = 
    let 
        helper2x2 x y z q = if (x + q > 5) then helper2x2 1 (y+1) z q
            else if (y + q > 5)
                then True
                else (not (((selector5x5 x y z) == (selector5x5 (x+q) (y+q) z)) && ((selector5x5 x y z) == (selector5x5 (x+q) (y) z)) 
                    && ((selector5x5 x y z) == (selector5x5 (x) (y+q) z)))) && helper2x2 (x+1) y z q
    in helper2x2 1 1 z 4 && helper2x2 1 1 z 3 && helper2x2 1 1 z 2 && helper2x2 1 1 z 1
--Possible Solution
k = Board ((False, False, False, True, True), (True, False, True, False, False), (False, True, True, True, False), (False,True,False, False, True), (True,True,True,False,False))