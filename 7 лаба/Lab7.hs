--import System.IO as IO (hClose, openFile, IOMode(ReadWriteMode) )
import Data.Char as C ()
import qualified System.IO.Strict as StrictIO
import Data.Text (replace, pack, findIndex)
import Data.Map (fromListWith, toList)
import Data.List
import Data.Maybe
import qualified Data.Map as M (Map, empty, insert, lookup)



main :: IO ()
main = do
  let file = "text.txt"
  let str = "hello i" ++ "\n"++"I am to be perfect one"++"\n"++"joy"++"\n"++"to be or not to be"++"\n"++"134"++"\n"++"567"
  putStrLn "Check output!"
  writeFile file (take (length str) (cycle str))
  content <- StrictIO.readFile file
  putStrLn "Content of input file\n"
  putStr content
  let b= lines content
  let m= [((wordsWhen (==' ') (b !! n)))| n <- [0,1..(length b)-1]]
  
  putStrLn "\n"
  putStrLn"Enter step of coding: "
  num'    <- getLine
  let num =  read num' :: Int
  let n m x  = head $ filter ((== x) . (m !!)) [0..]
  let p= [[if (((n m x) `mod` 2/=0)&&((n x i) `mod` 2/=0)) then (rotStr num i) else i |  i<-x]|x <-m]
  let p2=[intersperse " " i|i<-p]
  let p1= intersperse ["\n"] p2
  let gen = concat(concat p1)
  writeFile file (take (length gen) (cycle gen))
  
  putStrLn "\nContent of coded file\n"
  putStr gen
  putStrLn "\n"

wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'

alnum = ['A' .. 'Z']++['A' .. 'Z'] ++['A' .. 'Z'] ++['A' .. 'Z'] ++['a' .. 'z']++['a' .. 'z']
anlen = length alnum
 
indices = foldr add M.empty $ zip alnum [0..]
  where add (c, i) m = M.insert c i m
 
charsByIndex = foldr add M.empty $ zip alnum [0..]
  where add (c, i) m = M.insert i c m
 
rotChar :: Int -> Char -> Char
rotChar r c = case M.lookup c indices of
                   Just i  -> fromJust $ M.lookup (newIndex i) charsByIndex
                   Nothing -> c
              where newIndex i = (i+r) `mod` anlen
 
rotStr :: Int -> String -> String
rotStr r s = map (rotChar r) s