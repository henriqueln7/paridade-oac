-- Henrique Lopes Nóbrega, 119110534
import Data.Bits
-- Tipo representando a paridade.
data Parity = Even | Odd


-- O código pode ser visto melhor em https://github.com/henriqueln7/paridade-oac
-- Função responsável por verificar se a paridade de uma cadeia de bits (escolhi utilizar cada bit como um Int). É possível passar dois tipos de paridade: Even e Odd. Even retornará True caso o número de bits 1 seja par e Odd retornará True caso o número de bits seja ímpar.
checkParity :: [Integer] -> Parity -> Bool
checkParity bits Even = even $ sum bits
checkParity bits Odd = odd $ sum bits

-- Função que detecta utilizando o algoritmo de hamming onde ocorreu o erro. Se retornar 0 indica que não houve erro
hammingCheckErrorPosition :: [Int] -> Int
hammingCheckErrorPosition bits = foldl xor 0 (getIndexsBitActive bits)

-- Recebe uma sequência de bits e identifica se há erro. Se houver, faz a correção.
hammingCorrector :: [Int] -> [Int]
hammingCorrector bits
    | hammingCheckErrorPosition bits == 0 = bits
    | otherwise = invertBitAtPosition bits errorPosition
        where errorPosition = hammingCheckErrorPosition bits

-- Função auxiliária para inverter o bit em determinada posição
invertBitAtPosition :: [Int] -> Int -> [Int]
invertBitAtPosition [] _ = []
invertBitAtPosition (b:bs) position
    | position == 0 = myComplementBit b : bs
    | otherwise = b : invertBitAtPosition bs (position - 1)

-- Uma função auxiliária determinando o complemento dos inteiros
myComplementBit :: Int -> Int
myComplementBit 0 = 1
myComplementBit 1 = 0

-- Retorna uma lista onde os bits são ativos (bit = 1)
getIndexsBitActive :: [Int] -> [Int]
getIndexsBitActive bits = getIndexsBitActiveAux bits 0 []
    
-- Auxiliar para a função acima. Ajuda na recursão :)
getIndexsBitActiveAux :: [Int] -> Int -> [Int] -> [Int]
getIndexsBitActiveAux bits index result
        | index == length bits = result
        | otherwise = if bits !! index == 1 
            then getIndexsBitActiveAux bits (index + 1) result ++ [index] 
            else getIndexsBitActiveAux bits (index + 1) result