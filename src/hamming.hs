import Data.Bits

hammingCheckErrorPosition :: [Int] -> Int
hammingCheckErrorPosition bits = foldl xor 0 (getIndexsBitActive bits)

-- hammingCorrector :: [Int] -> [Int]
-- hammingCorrector bits
--     | hammingCheckErrorPosition bits == 0 = bits
--     | otherwise = invertBitAtPosition bits errorPosition
--         where errorPosition = hammingCheckErrorPosition bits

-- invertBitAtPosition :: [Int] -> Int -> [Int]
-- invertBitAtPosition bits position
--     | position == 0 = myComplementBit (head bits) : tail bits
--     | otherwise = invertBitAtPosition bits (position - 1)
-- myComplementBit :: Int -> Int
-- myComplementBit 0 = 1
-- myComplementBit 1 = 0

getIndexsBitActive :: [Int] -> [Int]
getIndexsBitActive bits = aux bits 0 []
    
aux :: [Int] -> Int -> [Int] -> [Int]
aux bits index result
        | index == length bits = result
        | otherwise = if bits !! index == 1 
            then aux bits (index + 1) result ++ [index] 
            else aux bits (index + 1) result