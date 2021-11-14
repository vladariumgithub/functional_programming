
int combine f a b n = sum $ map g [0..n - 1] where
    h    = (b - a) / n
    lo i = a + h * i
    g i  = h * combine (f $ lo i) (f $ lo i + h/2) (f $ lo i + h)

intSimp = int (\l m h -> (l + 4 * m + h) / 6)

main = do
          let a = "The definite integral (Simpson`s rule) of a function f(x)=(cos x)/(sqrt(1+x^2)): "  
          putStrLn $ a
          print $ intSimp f 0 3.14 10000
          
       where f x = (cos x)/(sqrt(1+x^2))