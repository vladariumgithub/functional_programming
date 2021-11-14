import Data.List ( nub, sort ) 

--перетворюємо вхідні дані в матрицю елементів, де (коєфіцієнт, степінь) для подальшої обробки
inpPoly :: String -> Bool -> Int -> Int -> Int -> [(Int,Int)] 
inpPoly [] _ s c p = [(s*c,p)]
inpPoly (x:xs) f s c p | f && (x == 'x') = inpPoly xs True s (if c==0 then 1 else c) 1
                       |      (x == '-') = (c*s,p) : (inpPoly xs True (-1) 0 0)
                       |      (x == '+') = (c*s,p) : (inpPoly xs True 1 0 0)
                       | f && (x == '^') = inpPoly xs False s c 0
                       | f               = inpPoly xs f s (c*10+(read [x] :: Int)) p
                       | otherwise       = inpPoly xs f s c (p*10+(read [x]  :: Int))                       

--відкидаємо не потрібні коєфіцієнти (==0)
simplify :: [(Int,Int)] -> [(Int,Int)]
simplify x = filter ((/= 0).fst) rlist
             where plist = sort $ nub $ map snd x
                   rlist = map (\ n -> (sum (map fst (filter ((== n).snd) x)), n)) plist

--перетворюємо вхідну матрицю на символьний вираз                 
showPoly :: [(Int,Int)] -> String
showPoly []         = ""
showPoly ((c,p):xs) | (p == 0)               = show(c) ++ showPoly xs
                    | (c == 1) && (p == 1)   = "+x" ++ showPoly xs
                    | (c == -1) && (p == 1)  = "-x" ++ showPoly xs
                    | (c == 1) && (p /= 1)   = "+x^" ++ show(p) ++ showPoly xs
                    | (c == -1) && (p /= 1)  = "-x^" ++ show(p) ++ showPoly xs
                    | (p /= 1)               = (if (c >0) then "+" else "") ++ show(c)++"x^" ++ show(p) ++ showPoly xs
                    | otherwise              = (if (c >0) then "+" else "") ++ show(c)++"x"  ++ showPoly xs                    

     
derivPoly :: [(Int,Int)] -> [(Int,Int)]
derivPoly = simplify . map (\ (c,p) -> (p*c,p-1)) 

--обробка суми поліномів                
addPoly :: [(Int,Int)] -> [(Int,Int)] -> [(Int,Int)]
addPoly x y = simplify $ x ++ y 

--обробка множення поліномів
multPoly :: [(Int,Int)] -> [(Int,Int)] -> [(Int,Int)]
multPoly x y = simplify $ foldl (\ acc c -> (map (\ yy -> ((fst yy)*(fst c),(snd yy)+(snd c))) y) ++ acc) [] x  

dp :: String -> String
dp x = showPoly $ derivPoly $ inpPoly x True 1 0 0

--вивід множення
mp :: String -> String -> String
mp x y = showPoly $ multPoly (inpPoly x True 1 0 0) (inpPoly y True 1 0 0)                          

-- вивід додавання
sp :: String -> String -> String
sp x y = showPoly $ addPoly (inpPoly x True 1 0 0) (inpPoly y True 1 0 0)                          
                    


main = do
    --вивід роботи функції inpPoly і simplify
    --print(inpPoly "-x^2+3x" True 1 0 0)
    --print(simplify $ inpPoly "-x^2+3x" True 1 0 0)

    putStrLn("Sum of polinoms: ")
    putStrLn(sp "x^2+3x" "x-5")
    putStrLn("   ")
    putStrLn("Product of polinoms: ")
    putStrLn(mp "x^2+3x" "x-5")
    
