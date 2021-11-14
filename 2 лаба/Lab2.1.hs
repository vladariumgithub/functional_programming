main :: IO ()
main= do 
    putStr result
    where
        result = concat [" __ __ __ __ __ __ __ __ __ __ __"++"\n"++"|X = "++ show(i) ++"\n"++"|Y("++show(i)++") = "++ y i ++"\n"++"|Y_control("++show(i)++") = "++ y_control i++ "\n"++"|Difference= "++ show((y1_control i- y1 i))++ "\n" | i <- [-2,-1.5..2]]


taylor k_ini k_n x = k_ini x * foldr (.) id (map (app k_n x) [2..100]) 1 
          where app k x n = (1+) . (k x n *)


cos_my x = taylor (\x->1) k_n x 
          where k_n x n = - x^2 / fromIntegral ((2*n-3)*(2*n-2))

y x
    | x>=(-1) && x<=0 = show((cos_my(x/2))/(cos_my(x^2)))
    | x>0 = show(((cos_my(x/2))^2)/(cos_my(2*x)))
    | otherwise     = show "Not in range"
y1 x
    | x>=(-1) && x<=0 = ((cos_my(x/2))/(cos_my(x^2)))
    | x>0 = (((cos_my(x/2))^2)/(cos_my(2*x)))
    | otherwise     = 0

y_control x
    | x>=(-1) && x<=0 = show((cos(x/2))/(cos(x^2)))
    | x>0 = show(((cos(x/2))^2)/(cos(2*x)))
    | otherwise     = show "Not in range"

y1_control x
    | x>=(-1) && x<=0 = ((cos(x/2))/(cos(x^2)))
    | x>0 = (((cos(x/2))^2)/(cos(2*x)))
    | otherwise     = 0
