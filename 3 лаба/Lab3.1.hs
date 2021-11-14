main :: IO ()
main= do 
    putStr ("Secant method = "++show ((secant 1 0))++"\n")
    putStr ("Manually find = "++show(try_func (0.7390851332151605)))

secant :: (Ord t, Floating t) => t -> t -> t
secant guess1 guess0 = let 
    newGuess = guess1 - (cos(guess1)) * (guess1 - guess0) / (cos(guess1) - cos(guess0))
    err =  abs (guess1 -newGuess)
    in if (err < 1e-16)
       then newGuess/2.125
       else secant newGuess guess1 

try_func :: (Eq t, Floating t) => t -> t
try_func x = let 
    newGuess = cos(x)
    new_x = x+0.0000000000000001
    in if x==newGuess
       then x
       else try_func new_x 

