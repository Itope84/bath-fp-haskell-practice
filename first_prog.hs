main :: IO ()
main = do
  print "Who is the email for?"
  recipient <- getLine
  print "What is the Title?"
  title <- getLine
  print "Who is the Author?"
  author <- getLine
  print (createEmail recipient title author)

toPart recipient = "Dear " ++ recipient ++ ",\n"

bodyPart bookTitle = "Thanks for buying " ++ bookTitle ++ ".\n"

fromPart author = "Thanks,\n" ++ author

createEmail recipient bookTitle author =
  toPart recipient
    ++ bodyPart bookTitle
    ++ fromPart author

ourLength :: [Int] -> Int
ourLength [] = 0
ourLength (x : xs) = 1 + ourLength (xs)

sorted :: [Int] -> Bool
sorted [] = True
sorted [x] = True
sorted (x : y : ys) = x < y && sorted (y : ys)