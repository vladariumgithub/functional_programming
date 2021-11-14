main :: IO()
main = do
    putStrLn "Enter sum: "
    sum'    <- getLine
    let sum =  read sum' :: Float

    putStrLn "Enter pr: "
    pr'    <- getLine
    let pr =  read pr' :: Float

    putStrLn "Enter m: "
    m'    <- getLine
    let m =  read m' :: Int
    
    let f' = func sum (pr/100.0) m 0
    putStrLn ("Sum = " ++ show (fst f'))
    putStrLn ("Recursion depth is " ++ show (snd f'))



func :: (Eq t1, Num t1, Num t2, Num t3) => t2 -> t2 -> t1 -> t3 -> (t2, t3)
func sum pr m d
    | m == 0        = (sum, d)
    | otherwise     = func (sum+sum*pr) pr (m-1) (d+1)


