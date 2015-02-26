import Data.Digits
import Data.List
import System.Environment

class Split a where
    split :: a -> [a]

instance Split Integer where
    split = digits 10

-- Actual run-length encoding.

runlengthList :: Eq a => [a] -> (Int,a)
runlengthList xs = (length xs, head xs)

runlength :: Eq a => [a] -> [(Int, a)]
runlength = f . group
    where   f = map runlengthList

runlengthSplit :: (Split a, Eq a) => a -> [(Int, a)]
runlengthSplit = runlength . split

-- Expression (turning run-length encoded input into human-readable Strings)

expressSingle :: Int -> String
expressSingle x
        | x < 10    = ns !! x
        | otherwise = show x
    where
        ns = ["zero", "one", "two", "three", "four",
              "five", "six", "seven", "eight", "nine"]

expressGroup :: (Show a, Eq a) => (Int, a) -> String
expressGroup (count, noun)
        | count == 1 = phrase ++ "."
        | otherwise  = phrase ++ "s."
    where
        phrase = expressSingle count ++ " " ++ show noun

expressList :: (Show a, Eq a) => [a] -> [String]
expressList = map expressGroup . runlength

express :: (Show a, Eq a) => [a] -> String
express = unwords . expressList

expressSplit :: (Split a, Show a, Eq a) => a -> String
expressSplit = express . split

main :: IO ()
main = getArgs >>= print . runlength . head

