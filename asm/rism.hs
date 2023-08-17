import System.IO
import Data.Int

genTypeR :: [String] -> [String]
genTypeR strList =  [] ++ (drop 4 strList)
    where
        machineCode = []

data InstructionType = Type ([String] -> [String])
data Instruction = Instruction {instructionName :: String,instructionType :: InstructionType,func7 :: String,func3 :: String,opcode :: String}
--instructionSet = [  Instruction {instructionName = "add",instructionType = TYPE_R},
  --                  Instruction {instructionName = "sub",instructionType = TYPE_R}  ]

fromBinaryToDecimal :: [Char] -> Int
fromBinaryToDecimal binary = foldl (\acc x -> 2*acc + x) 0 $ map (\x -> read [x]) binary

main = do
    handle <- openFile "code.asm" ReadMode
    contents <- hGetContents handle
    let token :: [String] = map (flip removeStrListFromStr ["\n"]) $ splitWithDeliChar contents [' ',',','(',')']
    let func = [genADD,genSUB]
    -- print $ zipWithFunction func token
    print token
    --print $ tokenize token func
    hClose handle

tokenize :: [String] -> [([String] -> [String])] -> [String]
tokenize strList funcList = tokenize' strList funcList [[]]
    where
        tokenize' :: [String] -> [([String] -> [String])] -> [String] -> [String]
        tokenize' [] _ tmp = tmp
        tokenize' strList@(str:strList') instructionList@(instruction:instructionList') tmp
            |   not $ null $ head result    = tokenize' resStrList instructionList (tmp ++ machineCode)
            |   otherwise                   = tmp
            where
                result      = mapFunc strList instructionList
                resStrList  = drop 4 result
                machineCode = take 4 result

genCode :: [String] -> [Instruction] -> [String]
genCode _ [] = [[]]
genCode strList@(str:strList') instructionList@(instruction:instructionList')
    |   null $ head $ result    = genCode strList instructionList'
    |   otherwise               = result
    where
        result = [[]]

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

restoreRegisterNumber :: String -> Int
restoreRegisterNumber rs = case rs of
    "zero"  -> 0;   "ra"    -> 1;   "sp"    -> 2;   "gp"    -> 3;   "tp"    -> 4;
    "t0"    -> 5;   "t1"    -> 6;   "t2"    -> 7;   "s0"    -> 8;   "fp"    -> 8;
    "s1"    -> 9;   "a0"    -> 10;  "a1"    -> 11;  "a2"    -> 12;  "a3"    -> 13;
    "a4"    -> 14;  "a5"    -> 15;  "a6"    -> 16;  "a7"    -> 17;  "s2"    -> 18;
    "s3"    -> 19;  "s4"    -> 20;  "s5"    -> 21;  "s6"    -> 22;  "s7"    -> 23;
    "s8"    -> 24;  "s9"    -> 25;  "s10"   -> 26;  "s11"   -> 27;  "t3"    -> 28;
    "t4"    -> 29;  "t5"    -> 30;  "t6"    -> 31;