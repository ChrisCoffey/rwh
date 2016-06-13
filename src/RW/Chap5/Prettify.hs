module RW.Chap5.Prettify 
 where

import Data.Bits    (shiftR, (.&.))
import Data.Char    (ord)
import Data.List    (intercalate)
import Numeric      (showHex)

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
string = enclose '"' '"' . hcat . map oneChar

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
  where mustEscape x = x < ' ' || x == '\x7f' || x > '\xff'

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
punctuate _ []  = []
punctuate _ [d] = [d]
punctuate p (d:ds) = (d <> p): punctuate p ds

compact :: Doc -> String
compact d = transform [d]
  where transform [] = ""
        transform (x:xs) = 
          case x of 
            Empty           -> transform xs
            Line            -> '\n' : transform xs
            Char c          -> c : transform xs
            Text s          -> s ++ transform xs
            a `Concat` b    -> transform (a:b:xs)
            _ `Union` b     -> transform (b:xs)

pretty :: Int -> Doc -> String
pretty width x = 
  best 0 [x]
  where best col (d:ds) = 
          case d of 
            Empty         -> best col ds
            Char c        -> c : best (col + 1) ds
            Text s        -> s ++ best (col + length s) ds
            Line          -> '\n' : best 0 ds
            a `Concat` b  -> best col (a:b:ds)
            a `Union` b   -> nicest col (best col (a:ds)) (best col (b:ds))
        
        best _ _ = ""

        nicest col a b 
          | (width - least) `fits` a = a
          | otherwise                = b
          where least = min width col

fits :: Int -> String -> Bool
w `fits` _ | w < 0  = False
_ `fits` ""         = True
_ `fits` ('\n':_)     = True
w `fits` (_:cs)     = (w -1) `fits` cs


fitsD :: Int -> Doc -> Bool
w `fitsD` _ | w < 0  = False
w `fitsD` d = 
  case d of
    Empty         -> True
    Line          -> True
    Char _        -> w > 1
    Text s        -> (length s) < w
    a `Concat` b  -> w `fitsD` a && w `fitsD` b
    a `Union` b   -> w `fitsD` a || w `fitsD` b

-- Fill should accept a number, and if a line break occurs before the document reaches
-- that width, left padd the line with n spaces
fill :: Int -> Doc -> Doc
fill width x =
  lp 0 [x] []
  where lp col (d:ds) dq = 
          case d of
            Empty           -> lp col ds dq
            Char _          -> lp (col + 1) ds (d:dq)
            Text s          -> lp (col + length s) ds (d:dq)
            Line
              | col < width -> lp width (d:ds) ((Text spaces):d:dq)
              | otherwise   -> ((hcat . reverse $ dq)) </> lp 0 ds []
            a `Concat` b    -> lp col (a:b:ds) dq
            a `Union` b     -> nicest col (lp col (a:ds) dq) (lp col (b:ds) dq)
            where 
              spaces = take (width - col) . repeat $ ' '
        lp _ _ q = hcat . reverse $ q
        nicest col a b
          | (width - least) `fitsD` a  = a
          | otherwise                 = b
          where 
            least = min width col


-- To extend this to actually support nesting, I'll need to track the columns where each opening character occurs
--  This can be done with another where function
nudge :: Int -> Doc -> Doc
nudge width x = 
  f 0 [x] []
  where f indent (d:ds) dq = 
          case d of
            Empty               -> f indent ds dq
            Char c 
              | isOpening c     -> f (indent + width)  (Line:ds) (d:dq)
              | isClosing c     -> f (indent - width)  (Line:ds) (d:dq)
              | otherwise       -> f indent ds (d:dq)
            Text s              -> f indent ds (d:dq)
            Line                -> (hcat . reverse $ dq) </> (Text spaces) <> f indent ds []
            a `Concat` b        -> f indent (a:b:ds) dq
            a `Union` _         -> f indent (a:ds) dq
          where spaces = replicate indent ' '
        f _ _ q = hcat . reverse $ q
        isOpening c = c == '[' || c == '{' || c == '(' 
        isClosing c = c == ']' || c == '}' || c == ')' 
