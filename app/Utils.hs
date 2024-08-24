module Utils where
import Text.Regex.TDFA
import Data.Char

pre (a,_,_,_) = a
post (_,_,a,_) = a
mchGrp (_,_,_,a) = a

getMatch :: String -> String -> (String, String, String, [String])
getMatch arg pat = arg =~ pat :: (String, String, String, [String])

getFrstMatchGrp :: (String, String, String, [String]) -> String
getFrstMatchGrp = getMatchGrp 0

getMatchGrp :: Integer -> (String, String, String, [String]) -> String
getMatchGrp 0 (_,_,_,x:xs) = x
getMatchGrp _ (_,_,_,[]) = []
getMatchGrp n (a,b,c,x:xs) = getMatchGrp (n-1) (a,b,c,xs)

-- Source https://en.wikipedia.org/wiki/Trimming_(computer_programming)#Haskell
stripSpace :: String -> String
stripSpace = f . f
   where f = reverse . dropWhile isSpace

replaceSpace :: String -> String
replaceSpace (' ':cs) = ['_'] ++ replaceSpace cs
replaceSpace (c:cs) = [c] ++ replaceSpace cs
replaceSpace [] = []
