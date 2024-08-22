import Text.Regex.TDFA
import Data.Char

getMatch :: String -> String -> (String, String, String, [String])
getMatch arg pat = arg =~ pat :: (String, String, String, [String])

getFrstMatchGrp :: (String, String, String, [String]) -> String
getFrstMatchGrp (_,_,_,x:xs) = x
getFrstMatchGrp (_,_,_,[]) = []

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

p_section = "^#([^#]*)$"
l_section :: String -> [String]
l_section arg = ["\\section{" ++ match ++ "}","\\label{sec:" ++ matchNospace ++ "}"]
  where match = getFrstMatchGrp $ getMatch arg p_section
        matchNospace = replaceSpace $ stripSpace match

p_subsection = "^##([^#]*)$"
l_subsection :: String -> [String]
l_subsection arg = ["\\subsection{" ++ match ++ "}","\\label{sec:" ++ matchNospace ++ "}"]
  where match = getFrstMatchGrp $ getMatch arg p_subsection
        matchNospace = replaceSpace $ stripSpace match

p_subsubsection = "^###([^#]*)$"
l_subsubsection :: String -> [String]
l_subsubsection arg = ["\\subsubsection{" ++ match ++ "}","\\label{sec:" ++ matchNospace ++ "}"]
  where match = getFrstMatchGrp $ getMatch arg p_subsubsection
        matchNospace = replaceSpace $ stripSpace match

pre (a,_,_,_) = a
post (_,_,a,_) = a
mchGrp (_,_,_,a) = a

--sed -i -E "s//\\\\textbf\{\1\}/g"  "$OUTPUT_FILE"
p_boldfont = "\\*\\*([^*]*)\\*\\*"
l_boldfont arg
  | mchGrp matches == [] = pre matches
  | otherwise = (pre matches) ++ "\\textbf{" ++ getFrstMatchGrp matches ++ "}" ++ (l_boldfont $ post matches)
  where matches = getMatch arg p_boldfont

p_italicfont = "([^*]|^)\\*([^*]*)\\*([^*]|$)"
l_italicfont arg
  | mchGrp matches == [] = pre matches
  | otherwise = (pre matches) ++ (getMatchGrp 0 matches) ++ "\\textit{" ++ (getMatchGrp 1 matches) ++ "}" ++ (getMatchGrp 2 matches) ++ (l_italicfont $ post matches)
  where matches = getMatch arg p_italicfont

parceAndReplace :: String -> [String]
parceAndReplace arg
  | arg =~ p_section = l_section arg
  | arg =~ p_subsection = l_subsection arg
  | arg =~ p_subsubsection = l_subsubsection arg
  | otherwise = [arg]

maps :: [String] -> [String]
maps (str:strs) = parceAndReplace str ++ maps strs
maps [] = []

main :: IO ()
main = interact $ unlines . maps . lines
