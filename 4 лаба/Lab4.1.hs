main :: IO ()
main = do
    putStrLn "Enter n: "
    n'    <- getLine
    let n =  read n' :: Int
    let list = take n [3,6..]
    putStrLn ("Given array: "++show(list))

    putStrLn "Enter number of element to add: "
    num' <- getLine
    let num= read num':: Int
    putStrLn "Enter element to add: "
    m <- intArray num
    
    putStrLn "Enter position of an element: "
    y'    <- getLine
    let y =  read y'-1 :: Int
    let ys = take y [3,6..]
    let zs = take (n-y) [3*y+3,3*y+6..]
    let list1=ys ++m++zs
    putStrLn ("Given array with added element: "++show(list1))
    
    let list2 = filter (\ x -> x `mod` 2 == 0) list1
    putStrLn ("Number of even numbers: "++show(length list2))
    let ser= div (sum list1) (length list1)
    
    
    let list3 = [if (x `mod` 2 == 0) then ser else x | x <- list1]
    putStrLn ("New modified array: "++show(list3))
    
intArray 0 = return []
intArray x = do
    str <- getLine
    nextInt <- intArray (x - 1)
    let int = read str :: Int
    return (int:nextInt)
 


    