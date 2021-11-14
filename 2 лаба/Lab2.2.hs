import Data.List (unfoldr)
import Data.Char (intToDigit)
import GHC.Types ( Any )

-- continued fraction represented as a (possibly infinite) list of pairs
napier :: [(Integer, Integer)]
napier = zip (1 : [3,5 ..]) [1,1 ..]

-- approximate a continued fraction after certain number of iterations
approxCF
  :: (Integral a, Fractional b)
  => Int -> [(a, a)] -> b
approxCF t = foldr (\(a, b) z -> fromIntegral a + fromIntegral b / z) 1 . take t

-- infinite decimal representation of a real number
decString
  :: RealFrac a
  => a -> String
decString frac = show i ++ '.' : decString_ f
  where
    (i, f) = properFraction frac
    decString_ = map intToDigit . unfoldr (Just . properFraction . (10 *))

main :: IO ()
main =
    mapM_
    
    (putStrLn .take (1+12) . decString . (approxCF 2000 :: [(Integer, Integer)] -> Rational))
    [napier]