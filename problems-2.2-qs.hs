-- Types in haskell are like typescript and Java (or any oop) had a baby. It has generic types like in typescript. Typescript: type Someting<T>, haskell: data Something t.
-- Java has constructors for classes (which can serve as types), haskell does too: (constructor) Duck(String a, Int b), haskell: data Whatever = Duck String Int (in java constructors have to have the class name, in haskell, it's not necessary)

-- Duck (on the rhs) is the type constructor and it simply defines its params

-- Types can also have multiple constructors, like in Duck below

data Duck = Duck String Int Float | Duckling String Int Float
  -- The Show interface can be overriden and it's like the toString function in many languages like Java
  deriving (Show)

donald :: Duck
donald = Duck "Donald" 6 0.63

daisy :: Duck
daisy = Duck "Daisy" 5 0.56

huey :: Duck
huey = Duckling "Huey" 2 0.23

dewey :: Duck
dewey = Duckling "Dewey" 2 0.25

duckFamily :: [Duck]
duckFamily = [donald, daisy, huey, dewey]

birthday :: Duck -> Duck
-- if we want to avoid code duplication, we can simply create a new function that accepts the unwrapped arguments
birthday (Duckling name age height)
  | age + 1 >= 3 = Duck name (age + 1) height
  | otherwise = Duckling name (age + 1) height
birthday (Duck name age height) = Duck name (age + 1) height

tall :: Duck -> Bool
tall (Duckling _ _ height) = height >= 0.25
tall (Duck _ _ height) = height >= 0.6
