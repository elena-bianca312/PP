import System.Random

x = fst $ next $ mkStdGen 42

check = do print x; print "Install OK"