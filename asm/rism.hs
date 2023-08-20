import System.IO
import Data.Int

genTypeR :: [String] -> [String]
genTypeR strList =  [] ++ (drop 4 strList)
    where
        machineCode = []

data InstructionType = TYPE_R | TYPE_O
data Instruction = Instruction {instructionName :: String,instructionType :: InstructionType,func7 :: String,func3 :: String,opcode :: String}
--instructionSet = [  Instruction {instructionName = "add",instructionType = TYPE_R},
  --                  Instruction {instructionName = "sub",instructionType = TYPE_R}  ]

fromBinaryToDecimal :: String -> Int
fromBinaryToDecimal binary = foldl (\acc x -> 2*acc + x) 0 $ map (\x -> read [x]) binary

main = do
    handle <- openFile "code.asm" ReadMode
    contents <- hGetContents handle
    let token :: [String] = map (flip removeStrListFromStr ["\n"]) $ splitWithDeliChar contents [' ',',','(',')']
    -- let func = [genADD,genSUB]
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

mapFunc :: [String] -> [([String] -> [String])] -> [String]
mapFunc _ [] = [[]]
mapFunc strList@(str:strList') funcList@(func:funcList')
    |   null $ head $ result    = mapFunc strList funcList'
    |   otherwise               = result
    where
        result = func strList

genCode :: [String] -> [Instruction] -> [String]
genCode _ [] = [[]]
genCode strList@(str:strList') instructionList@(instruction:instructionList')
    |   null $ head $ result    = genCode strList instructionList'
    |   otherwise               = result
    where
        result = genMachineCode strList instruction

genMachineCode :: [String] -> Instruction -> [String]
genMachineCode strList@(str:strList') instruction
    |   instructionName instructionType == TYPE_R   = [[]] 
    |   otherwise                           = [[]]
    where

genR :: [String] -> Int -> Int -> Int -> [String]
genR asm func7 func3 opcode = fromDecimalToBinary $ func7 `shiftL` 25 + rs2 `shiftL` 20 + rs1 `shiftL` 15 + func3 `shiftL` 12 + rd `shiftL` 7 + opcode
    where
        rs1 = restoreRegisterNumber $ head $ takePart asm 2 1
        rs2 = restoreRegisterNumber $ head $ takePart asm 3 1
        rd  = restoreRegisterNumber $ head $ takePart asm 1 1

fromDecimalToHex :: Int -> String
fromDecimalToHex de = fromDecimalToHex' de []
fromDecimalToHex' :: Int -> String -> String
fromDecimalToHex' de tmp
    |   remainder /= 0  = fromDecimalToHex' quotient tmp ++ [convTable !! remainder]
    |   otherwise       = tmp
    where
        quotient    = de `div` 16
        remainder   = de `mod` 16
        convTable   = ['0','1','2','3','4','5','6','7','8','9','a','b','c','d','e','f']

take' :: [a] -> [Int] -> [a]
take' a b = take'' a b []
take'' :: [a] -> [Int] -> [a] -> [a]
take'' _ [] a' = a'
take'' a b@(_b:b') a' = take'' a b' a' ++ [a !! _b]

takePart :: [a] -> Int -> Int -> [a]
takepart [] _ _ = []
takePart a start end = take end $ drop start a

shiftL :: Int -> Int -> Int
shiftL num sh = 2 ^ sh * num

fromDecimalToBinary :: Int -> [String]
fromDecimalToBinary deci = [takePart filled 0 2] ++ [takePart filled 2 2] ++ [takePart filled 4 2] ++ [takePart filled 6 2]
    where
        hex     = fromDecimalToHex deci
        filled  = (flip replicate '0' $ 8 - length hex) ++ hex

numLength :: Int -> Int
numLength num = numLength' num 1
    where
        numLength' :: Int -> Int -> Int
        numLength' num tmp
            |   num' > 0     = numLength' num' (tmp + 1)
            |   otherwise    = tmp
            where
                num' = num `div` 10



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