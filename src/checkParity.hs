data Parity = Even | Odd

-- Função responsável por verificar se a paridade de uma cadeia de bits (aqui foi escolhido representar cada bit como um char). É possível passar dois tipos de paridade: Even e Odd. Even retornará True caso o número de bits 1 seja par e Odd retornará True caso o número de bits seja ímpar.
checkParity :: [Integer] -> Parity -> Bool
checkParity bits Even = even $ sum bits
checkParity bits Odd = odd $ sum bits

