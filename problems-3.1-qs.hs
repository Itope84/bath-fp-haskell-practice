import Data.Char

---Maps

toLowerSt :: String -> String
toLowerSt = map toLower

includes :: [Char] -> Char -> Bool
includes [] _ = False
includes (x : xs) c
  | x == c = True
  | otherwise = includes xs c

toLowerCons :: Char -> Char
toLowerCons c
  | includes ['A', 'E', 'I', 'O', 'U'] c = c
  | otherwise = toLower c

toLowerConsSt = map toLowerCons

---Filters

onlyLetters :: String -> String
onlyLetters = filter isLetter

onlyNumsOrLetters :: [Char] -> [Char]
onlyNumsOrLetters = filter isDigitOrLetter
  where
    isDigitOrLetter c = isDigit c || isLetter c

onlyLettersToLower1 :: [Char] -> [Char]
onlyLettersToLower1 st = map toLower (filter isLetter st)

onlyLettersToLower2 :: [Char] -> [Char]
onlyLettersToLower2 st = filter isLetter (map toLower st)

---Zips

firstNames :: [String]
firstNames = ["Adam", "Brigitte", "Charlie", "Dora"]

secondNames :: [String]
secondNames = ["Ashe", "Brown", "Cook", "De Santis"]

wholeNames :: [(String, String)]
wholeNames = zip firstNames secondNames

countNames :: [String] -> [(Int, String)]
countNames = zip [1 ..]

wholeNames2 :: [String]
wholeNames2 = zipWith getName firstNames secondNames
  where
    getName f s = f ++ " " ++ s

rollCall :: [String]
rollCall = zipWith call xs firstNames
  where
    call i name = (show i) ++ ": " ++ name ++ "? 'Present!'"
    xs = [1 ..]
