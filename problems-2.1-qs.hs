import Data.Char

---Exercise 1

toUpperSt :: String -> String
toUpperSt [] = []
toUpperSt (c : cs) = (toUpper c) : (toUpperSt cs)

deleteDigits :: String -> String
deleteDigits [] = []
deleteDigits (c : cs)
  | isDigit c = deleteDigits cs
  | otherwise = c : deleteDigits cs

leetSpeak :: String -> String
leetSpeak [] = ['!']
leetSpeak (x : xs)
  | x == 'e' = '7' : leetSpeak xs
  | x == 'o' = '0' : leetSpeak xs
  | x == 's' = 'z' : leetSpeak xs
  | x == 'o' = '0' : leetSpeak xs
  | otherwise = x : leetSpeak xs

-- Exercise 2

factors2 :: Int -> [Int]
factors2 0 = []
factors2 n
  | (n `mod` 2 == 0) = 2 : factors2 (n `div` 2)
  | otherwise = [n]

factorsm :: Int -> Int -> [Int]
factorsm _ 0 = []
factorsm m n
  | (n `mod` m == 0) = m : factorsm m (n `div` m)
  | otherwise = [n]

factorsFrom :: Int -> Int -> [Int]
factorsFrom _ 0 = []
factorsFrom m n
  | m >= n = [n]
  | (n `mod` m == 0) = m : factorsFrom m (n `div` m)
  | otherwise = factorsFrom (m + 1) n

primeFactors :: Int -> [Int]
-- 2 is the smallest prime number, prime factors build from there
primeFactors = factorsFrom 2
