-- Part 1

isEven :: Int -> Bool
isEven n = n `mod` 2 == 0

square :: Int -> Int
square n = n * n

squareEven1 :: Int -> Int
squareEven1 n
  | n `mod` 2 == 0 = n * n
  | otherwise = n

squareEven2 :: Int -> Int
squareEven2 n
  | isEven n = square n
  | otherwise = n

-- Part 2

myOdd :: Int -> Bool
myOdd n
  | (n `mod` 2 == 0) = False
  | otherwise = True

grade :: Int -> String
grade n
  | n < 0 = "Marks cannot be negative"
  | n < 40 = "Fail"
  | n < 50 = "Low Pass"
  | n < 60 = "Medium Pass"
  | n < 70 = "High Pass"
  | n < 80 = "Merit"
  | n < 100 = "Distinction"
  | otherwise = "Invalid Mark"

-- Part 3

factorial :: Int -> Int
factorial n
  | n <= 1 = 1
  | otherwise = n * factorial (n - 1)

triangle :: Int -> Int
triangle n
  | n <= 1 = n
  | otherwise = n + triangle (n - 1)

total :: [Int] -> Int
total xs
  | (xs == []) = 0
  | otherwise = head xs + total (tail xs)

multiple :: [Int] -> Int
multiple xs
  | (xs == []) = 1
  | otherwise = head xs * multiple (tail xs)

triangle' :: Int -> Int
triangle' n = total [1 .. n]

factorial' :: Int -> Int
factorial' n = multiple [1 .. n]

-- euclid algo for greatest common divisor
euclid :: Int -> Int -> Int
euclid x y
  | x == y = x
  | otherwise = euclid (min x y) (abs (x - y))

-- Part 4
gcd5 = euclid 5

facDiv :: Int -> Int -> Int
facDiv m n
  | (m > n) = (factorial m) `div` (factorial n)
  | (m < n) = (factorial n) `div` (factorial m)
  | otherwise = 1

facDiv7 :: Int -> Int
facDiv7 = facDiv 7

facTri :: Bool -> Int -> Int
facTri b n
  | b = factorial n
  | otherwise = triangle n

facEvenTriOdd n = facTri (isEven n) n
