import System.IO
import Data.Int


main = do
    handle <- openFile "code.asm" ReadMode
    contents <- hGetContents handle
    let tokens :: [String] = ((split contents [] [[]] [' ',',']) `removeChar` [' '])
    print tokens
    hClose handle

split :: String -> String -> [String] -> [Char] -> [String]
split [] tmp t ch = t ++ [tmp]
split str@(s:str') [] [[]] ch = split str' [s] [] ch
split str@(s:str') tmp t ch
    |   checkElem [s] ch = split str' [] (t ++ [tmp]) ch
    |   otherwise = split str' (tmp ++ [s]) t ch

checkElem :: String -> [Char] -> Bool
checkElem [s] [] = False
checkElem [] _ = False
checkElem (s:str) (c:ch)
    |   s == c      = True
    |   otherwise   = checkElem [s] ch

removeChar :: [String] -> [Char] -> [String]
removeChar [] ch = []
removeChar str@(s:str') ch
    |   s `checkElem` ch    =   removeChar str' ch
    |   s == []             =   removeChar str' ch
    |   otherwise           =   [s] ++ removeChar str' ch

removeStrFromStrList:: [String] -> String -> [String]
removeStrFromStrList [] _ = []
removeStrFromStrList str_list@(s:str_list') remove_str
    |   sum' (zipWith (==) s remove_str) (==True)   = removeStrFromStrList str_list' remove_str
    |   otherwise                                   = [s] ++ removeStrFromStrList str_list' remove_str

isContain :: [Bool] -> Bool
isContain [] = False
isContain all@(a:all')
    |   a == True = True
    |   otherwise = isContain all'

sum' :: [a] -> (a -> Bool) -> Bool
sum' [] _ = True
sum' all@(a:all') f
    |   f a         = True && (sum' all' f)
    |   otherwise   = False

