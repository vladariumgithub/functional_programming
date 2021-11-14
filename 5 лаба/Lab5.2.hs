{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
import Data.Complex

import Control.Monad

main :: IO ()
main = do
    let j n m= m :+ n
    let x = j 3.0 0
    let y = j 6.0 2
    let z = j 5.0 1
    let n = j 2.0 2
    let m = j 6.0 3
    let list = take 5 [x,y,z,n,m]
    --putStrLn ("Array of complex numbers in alg. format: " ++ show(list))
    
    let list2 = [if x==x then (convertToTrigonometric x) else x | x <- list]
    
    
    putStrLn ("Array of complex numbers in alg. format: ")
    putStrLn(show(realPart(list !! 0))++"+i*"++show(imagPart(list !! 0))++", "++show(realPart(list !! 1))++"+i*"++show(imagPart(list !! 1))++", "++show(realPart(list !! 2))++"+i*"++show(imagPart(list !! 2))++", "++show(realPart(list !! 3))++"+i*"++show(imagPart(list !! 3))++", "++show(realPart(list !! 4))++"+i*"++show(imagPart(list !! 4)))
   
    putStrLn ("\nArray of complex numbers in trig. format: ")
    putStrLn((show(magnitude (list !! 0))) ++"(cos("++(show(phase (list !! 0)))++")+i*sin("++(show(phase (list !! 0)))++")), "++(show(magnitude (list !! 1))) ++"(cos("++(show(phase (list !! 1)))++")+i*sin("++(show(phase (list !! 1)))++")), "++(show(magnitude (list !! 2))) ++"(cos("++(show(phase (list !! 2)))++")+i*sin("++(show(phase (list !! 2)))++")), "++(show(magnitude (list !! 3))) ++"(cos("++(show(phase (list !! 3)))++")+i*sin("++(show(phase (list !! 3)))++")), "++(show(magnitude (list !! 4))) ++"(cos("++(show(phase (list !! 4)))++")+i*sin("++(show(phase (list !! 4)))++"))")
    putStrLn ("\nCalculated complex numbers in trig. format: " ++ show(list2))
    

convertToTrigonometric x = r * cos phi :+ r * sin phi
    where
        r = magnitude x
        phi = phase x