import qualified Data.Vector as V

main :: IO ()
main = do
    let a = V.fromList [2,3,4,5,6,7]
    let n = V.length (a) - 1
    Prelude.putStrLn("Given array: ")
    print (a)
    let evennum = V.fromList [(a V.! i) | i <-[0,2..n]]
    let oddnum = V.fromList [(a V.! i) | i <-[1,3..n]]
   
    let x1=V.minimum evennum
    let x2=V.maximum evennum
    let y1=V.minimum oddnum
    let y2=V.maximum oddnum
    let ix1=V.findIndices (==x1) a
    let ix2=V.findIndices (==x2) a
    let iy1=V.findIndices (==y1) a
    let iy2=V.findIndices (==y2) a
    Prelude.putStrLn("Min element of even numbers: "++Prelude.show(x1)++" with index "++Prelude.show(ix1))
    Prelude.putStrLn("Max element of even numbers: "++Prelude.show(x2)++" with index "++Prelude.show(ix2))
    Prelude.putStrLn("Min element of odd numbers: "++Prelude.show(y1)++" with index "++Prelude.show(iy1))
    Prelude.putStrLn("Max element of odd numbers: "++Prelude.show(y2)++" with index "++Prelude.show(iy2))
    
    
