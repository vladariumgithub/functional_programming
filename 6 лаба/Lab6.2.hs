import Data.Sequence (Seq(..), (<|), (|>), (><))
import qualified Data.Sequence as Seq
import qualified Data.Foldable as Foldable

main :: IO ()
main=do
    putStrLn "Given stack: "
    --добавляєм елементи, порядок виконання з права на ліво (тобто першим заходить 1)
    let a =10<| 9<| 8<| 7<| 6<| 5<| 4<| 3<| 2<| 1<| Seq.fromList []
    --let m= Seq.fromList[n|n <- a]
    print a
    
    ----let n= Seq.index b 5
    let b= Foldable.toList(Seq.reverse a)
    putStrLn "Enter number: "
    num' <- getLine
    let num= read num':: Int
    let list2 = filter (\ x -> x `mod` num /= 0) b
    let c=Seq.fromList list2
    putStrLn "Modified stack: "
    print c
    
