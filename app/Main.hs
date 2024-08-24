import Parsers
import Text.Regex.TDFA

parceAndReplace :: String -> [String]
parceAndReplace arg
  | arg =~ p_section = l_section arg
  | arg =~ p_subsection = l_subsection arg
  | arg =~ p_subsubsection = l_subsubsection arg
  | arg =~ p_boldfont = l_boldfont arg
  | arg =~ p_italicfont = l_italicfont arg
  | arg =~ p_citation = l_citation arg
  | otherwise = [arg]

mapOneline :: [String] -> [String]
mapOneline (str:strs) = parceAndReplace str ++ maps strs
mapOneline [] = []

main :: IO ()
main = interact $ unlines . mapOneline . lines
