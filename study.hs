import Prelude hiding (Maybe(..), isJust)

-- Problem 1: Define a data type MyNat, that represents numbers 0,1,2,...
--            Do so using the Peano encoding of numbers:
--
--            0 is a number (Hint: you can't make a constructor named 0. Try O?)
--            if n is a number, so is n + 1, or succ n.

data MyNat = YourDefinitionHere

-- Problem 2: Given a MyNat, return True if it's 0, and False if it's any other number.
isZero :: MyNat -> Bool
isZero = undefined

-- Problem 3: Write a function equal, which checks if two MyNat's are equal.
equal :: MyNat -> MyNat -> Bool
equal = undefined

-- Problem 4: Write a function add, which adds two MyNats.
add :: MyNat -> MyNat -> MyNat
add = undefined

-- Problem 5: Write a function inList, which checks if a number is in a list of numbers.
inList :: Int -> [Int] -> Bool
inList = undefined

-- Problem 6: Define Maybe a! There's nothing special about Maybe from the standard library.
--            You can just as well declare it yourself. Try it!
--            If you don't want to, feel free to find a definition of
--            Maybe online to do the last few problems.

data Maybe a = YourOtherDefinitionHere

-- Problem 7: Define a function isJust, that tells you if a Maybe a is "Just" something,
--            or "Nothing". It should return False for Nothing values, and True
--            for Just values.

isJust :: Maybe a -> Bool
isJust = undefined

-- Problem 8: Define a function "safeDivide", that divides one number by another number,
--            except when the other number is 0. When the number is 0, it should return
--            Nothing.
safeDivide :: Int -> Int -> Maybe Int
safeDivide = undefined

-- Problem 9: Define function "divideThenAdd", **using safeDivide**, that
--            divides one number by another. If the division is successful,
--            this function should add 1 to the result of the division, and return
--            that. If the division fails (you get Nothing), it should return 0.
divideThenAdd :: Int -> Int -> Int
divideThenAdd = undefined
