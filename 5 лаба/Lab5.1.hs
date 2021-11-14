import Data.Ratio ( Ratio, (%), denominator, numerator )
lcmMy x y = abs(x*y) `div` (gcdMy x y)

gcdMy n m | (m > n) = gcdMy m n
          | ((n `rem` m) == 0) = m
          | otherwise = gcdMy m (n `rem` m)

main :: IO ()
main = do
    putStrLn "Enter first rational number: "
    a' <- getLine
    let a= read a':: Int
    putStrLn "-"
    b' <- getLine
    let b= read b':: Int

    putStrLn "Enter second rational number: "
    c' <- getLine
    let c= read c':: Int
    putStrLn "-"
    d' <- getLine
    let d= read d':: Int
    let dr1=a%b
    let dr2=c%d
    putStr ("Given rational numbers: "++show(dr1)++", " ++show(dr2))
    let nsk=lcmMy (numerator dr1) (numerator dr2)
    let nsd=gcdMy (denominator dr1) (denominator dr2)
    --putStrLn(show(gcd b d)++" "++show(nsd))
    --putStrLn(show(lcm a c)++" "++show(nsk))
    putStrLn ("\nNSK: "++show(div nsk nsd))