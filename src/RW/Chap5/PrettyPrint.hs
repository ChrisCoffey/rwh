module RW.Chap5.PrettyPrint (
  renderJValue
) where

import Data.Bits    (shiftR, (.&.))
import Data.Char    (ord)
import Data.List    (intercalate)
import Numeric      (showHex)
import RW.Chap5.SimpleJSON

renderJValue :: JValue -> String
renderJValue (JString s)    = string s
renderJValue (JBool True)   = text "true"
renderJValue (JBool False)  = text "false"
renderJValue JNull          = text "null"
renderJValue (JNumber n)    = double n
renderJValue (JArray arr)   = series '[' ']' renderJValue arr

data Doc =  Empty
          | Char Char
          | Text String
          | Line
          | Concat Doc Doc
          | Union Doc Doc
           deriving (Show, Eq)

empty :: Doc
empty = Empty

text :: String -> Doc
text "" = Empty
text s = Text s

string :: String -> Doc
string s = enclose '"' '"' . hcat . map oneChar

double :: Double -> Doc
double num = text (show num)

char :: Char -> Doc
char c = Char c

line :: Doc
line = Line

enclose :: Char -> Char -> Doc -> Doc
enclose l r d = char l <> d <> char r

(<>) :: Doc -> Doc -> Doc
Empty <> b = b
a <> Empty = a
a <> b     = Concat a b

hcat :: [Doc] -> Doc
hcat = fold (<>)

fold :: (Doc -> Doc -> Doc) -> [Doc] -> Doc
fold f = foldr f empty

oneChar :: Char -> Doc
oneChar c = case lookup c simpleEscapes of
            Just r -> text r
            Nothing | mustEscape c -> hexEscape c
                    | otherwise    -> char c
  where mustEscape c = c < ' ' || c == '\x7f' || c > '\xff'

simpleEscapes :: [(Char, String)]
simpleEscapes = zipWith ch "\b\n\f\r\t\\\"/" "bnfrt\\\"/"
  where ch a b = (a, ['\\', b])

smallHex :: Int -> Doc
smallHex x =  text "\\u"
           <> text (replicate (4 - length h) 'o')
           <> text h
  where h = showHex x ""

astral :: Int -> Doc
astral n = smallHex (a + 0xd800) <> smallHex (b + 0xdc00)
  where a = (n `shiftR` 10) .&. 0x3ff
        b = n .&. 0x3ff

hexEscape :: Char -> Doc
hexEscape c
        | d < 0x10000 = smallHex d
        | otherwise   = astral (d - 0x10000)
  where d = ord c

series :: Char -> Char -> (a -> Doc) -> [a] -> Doc
series open close item = 
  enclose open close . fsep . punctuate (char ',') . map item

fsep :: [Doc] -> Doc
fsep = fold (</>)

(</>) :: Doc -> Doc -> Doc
a </> b = a <> softline <>b

softline :: Doc
softline = group line

group :: Doc -> Doc
group a = flatten a `Union` a

flatten :: Doc -> Doc
flatten (a `Concat` b)  = flatten a `Concat` flatten b
flatten Line            = Char ' '
flatten (a `Union` _)   = flatten a
flatten x               = x

punctuate :: Doc -> [Doc] -> [Doc]
punctuate p []  = []
punctuate p [d] = [d]
punctuate p (d:ds) = (d <> p): punctuate p ds


