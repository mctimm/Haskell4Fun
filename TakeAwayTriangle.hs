data Triangle = Triangle Int Int Int deriving (Show)

instance Eq Triangle where
    Triangle a b c == Triangle x y z = a == x && b == y && c == z
takeAway (Triangle x y z) = Triangle (abs(x-y)) (abs(y - z)) (abs(z - x))
sumTriangle (Triangle x y z) = x + y + z
takeAwayLoop num u = 
    let helper u@(Triangle x y z) num original = if sumTriangle u /= num
            then False
            else if any (u ==) original
                then True
                else helper (takeAway u) num (u:original)
    in helper (takeAway u) num [u]

takeAwayLoopFull n = 
    let helper n = [(Triangle x z y, x,y,z)| x<-[0..n], y<-[0..x], z<-[0..y], z + y + x == n]
    in map (\(x,y,z,a) -> (takeAwayLoop n x,y,z,a)) (helper n)