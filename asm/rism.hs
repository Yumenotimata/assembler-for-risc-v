import System.IO
import Data.Int

main = do
    handle <- openFile "code.asm" ReadMode
    contents <- hGetContents handle
    let token :: [String] = map (flip removeStrListFromStr ["\n"]) $ splitWithDeliChar contents [' ',',']
    let func = [genADD,genSUB]
    -- print $ zipWithFunction func token
    print $ tokenize token func
    hClose handle

tokenize :: [String] -> [([String] -> [String])] -> [String]
tokenize strList funcList = tokenize' strList funcList [[]]
    where
        tokenize' :: [String] -> [([String] -> [String])] -> [String] -> [String]
        tokenize' [] _ tmp = tmp
        tokenize' strList@(str:strList') funcList@(func:funcList') tmp
            |   not $ null $ head result    = tokenize' resStrList funcList (tmp ++ machineCode)
            |   otherwise                   = tmp
            where
                result      = mapFunc strList funcList
                resStrList  = drop 4 result
                machineCode = take 4 result

mapFunc :: [String] -> [([String] -> [String])] -> [String]
mapFunc _ [] = [[]]
mapFunc strList@(str:strList') funcList@(func:funcList')
    |   null $ head $ result    = mapFunc strList funcList'
    |   otherwise               = result
    where
        result = func strList

splitWithDeliChar :: String -> [Char] -> [String]
splitWithDeliChar str deli = splitWithDeliChar' str [] [[]] deli
    where
        splitWithDeliChar' :: String -> String -> [String] -> [Char] -> [String]
        splitWithDeliChar' [] tmp tmpList _ = filter (not . null) $ tmpList ++ [tmp]
        splitWithDeliChar' str@(s:str') tmp tmpList deli
            |   s `elem` deli   = splitWithDeliChar' str' [] (tmpList ++ [tmp]) deli
            |   otherwise       = splitWithDeliChar' str' (tmp ++ [s]) tmpList deli

removeStrListFromStr :: String -> [String] -> String
removeStrListFromStr str [] = str
removeStrListFromStr str rmvList@(rmv:rmvList') = flip removeStrListFromStr rmvList' $ removeStrFromStr str rmv

removeStrFromStr :: String -> String -> String
removeStrFromStr str [] = str
removeStrFromStr [] _ = []
removeStrFromStr str@(s:str') rmv
    |   foldl (&&) True $ zipWith (==) str rmv  = flip removeStrFromStr rmv $ drop (length rmv) str'
    |   otherwise                               = [s] ++ removeStrFromStr str' rmv

genADD :: [String] -> [String]
genADD strList@(str:strList')
    |   str == "add" = ["ADD","T","T","T"] ++ (drop 4 strList)
    |   otherwise    = [[]]

genSUB :: [String] -> [String]
genSUB strList@(str:strList')
    |   str == "sub" = ["SUB","S","S","S"] ++ (drop 3 strList)
    |   otherwise    = [[]]