import Data.Bits

hammingCheck :: [Int] -> Either String Int
hammingCheck bits = if indexError == 0 then Left "Nenhum erro ocorreu" else Right indexError
    where indexError = foldl xor 0 (getIndexsBitActive bits)

getIndexsBitActive :: [Int] -> [Int]
getIndexsBitActive bits = aux bits 0 []
    
aux :: [Int] -> Int -> [Int] -> [Int]
aux bits index result
        | index == length bits = result
        | otherwise = if bits !! index == 1 
            then aux bits (index + 1) result ++ [index] 
            else aux bits (index + 1) result