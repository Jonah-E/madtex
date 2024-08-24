module Parsers where
import Text.Regex.TDFA
import Data.Char
import Utils

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

p_boldfont = "\\*\\*([^*]*)\\*\\*"
l_boldfont :: String -> [String]
l_boldfont arg
  | mchGrp matches == [] = [pre matches]
  | otherwise = [(pre matches) ++ "\\textbf{" ++ mg0 ++ "}"] ++ nextMatch
  where matches = getMatch arg p_boldfont
        mg0 = getFrstMatchGrp matches
        nextMatch = l_boldfont $ post matches

p_italicfont = "([^*]|^)\\*([^*]*)\\*([^*]|$)"
l_italicfont :: String -> [String]
l_italicfont arg
  | mchGrp matches == [] = [pre matches]
  | otherwise = [(pre matches) ++ mg0 ++ "\\textit{" ++ mg1 ++ "}" ++ mg2 ++ nextMatch]
  where matches = getMatch arg p_italicfont
        mg0 = getMatchGrp 0 matches
        mg1 = getMatchGrp 1 matches
        mg2 = getMatchGrp 2 matches
        nextMatch = l_italicfont $ post matches

p_citation = " ?\\[\\[@([^]]*)\\]\\]"
l_citation arg
  | mchGrp matches == [] = [pre matches]
  | otherwise = [ppre ++ "~\\\\cite{" ++ pg0 ++ "}" ++ nextMatch]
  where matches = getMatch arg
        mpre = pre matches
        mg0 = getMatchGrp 0 matches
        mpost = post matches
        nextMatch = l_citation mpost

p_metaData = "^---"
ml_metaData :: [String] -> [String]
ml_metaData (x:xs)
  | x =~ p_metaData = dropWhile isMetaData xs
  | otherwise = [x] ++ ml_metaData xs
  where isMetaData arg = arg ~= p_metaData == False

