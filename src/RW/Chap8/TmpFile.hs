module RW.Chap8.TmpFile 
where

--Regexes are highly polymorphic & vary based on the return type of the regex function. 
-- List return are used to return all matches, Bools indicate the presence of a match, & 
-- String reponses return the match. A tripple returns (before, match, after), while a 
-- (String, String, String, [String]) tuple also provides a list of all groups in the pattern
-- that matched.

import Text.Regex.Posix             ((=~))

globToRegex :: String -> String
globToRegex cs = 
  '^':globToRegex' cs ++ "$"
  where 
    globToRegex' ""             = ""
    globToRegex' ('*':rest)     = ".*" ++ globToRegex' rest
    globToRegex' ('?':rest)     = "." ++ globToRegex' rest
    globToRegex' ('[':'!':rest) = "[^" ++ globToRegex' rest
    globToRegex' ('[':x:rest)   = '[':x: globToRegex' rest
    globToRegex' ('[':_)        = error "Unterminated character class"
    globToRegex' (x:rest)       = escape x ++ globToRegex' rest

escape :: Char -> String
escape c
  | c `elem` regexChars   = '\\':[c]
  | otherwise             = [c]
  where regexChars = "\\+()^$.{}]|"
