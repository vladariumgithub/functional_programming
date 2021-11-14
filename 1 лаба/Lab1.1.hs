main :: IO()
main = do
    putStrLn "Enter n: "
    n'    <- getLine
    let n =  read n' :: Int

    let f' = func (abs n) 
    putStrLn ("Sum = " ++ show f')
    putStrLn ("Recursion depth is " ++ show (depth (abs n) 0))

mod' :: (Ord t, Num t) => t -> t -> t
mod' x y | y > x     =  x
         | otherwise =  mod' (x-y) y

func :: Integral p => p -> p
func n
    | n == 0        = 0  
    | otherwise     = mod' n 10 + func(div n 10)

depth :: (Integral t1, Num t2) => t1 -> t2 -> t2
depth n d
    | n == 0        = d
    | otherwise     = depth(div n 10) (d+1)



    

    