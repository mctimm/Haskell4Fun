import System.Random
--x and y are the caps
numberMultpler x y z = (head multi) * (head (tail multi)) where multi = (randomRs (x,y) (mkStdGen z))
getTwoRandoms x z = ((head multi) , (head (tail multi))) where multi = (randomRs (2^x,(2^(x+1))-1) (mkStdGen z))
