import Data.Digits
import Data.List

-- Actual run-length encoding.

runlengthList :: Eq a => [a] -> (Int,a)
runlengthList xs = (length xs, head xs)

runlength :: Eq a => [a] -> [(Int, a)]
runlength x = map runlengthList $ group x

runlengthInt :: (Integral a) => a -> [(Int, a)]
runlengthInt = runlength . (digits 10)

runlengthChar :: Char -> [(Int, Char)]
runlengthChar c = runlength (c:[])

-- Expression (turning run-length encoded Integers into Strings)
expressSingle :: Int -> String
expressSingle x | x < 10    = ns !! x
                | otherwise = show x
                where ns = ["zero", "one", "two", "three",
                            "four", "five", "six", "seven",
                            "eight", "nine"]

expressGroup :: (Show a, Eq a) => (Int, a) -> String
expressGroup xs | (fst xs == 1) = phrase ++ "."
                | otherwise     = phrase ++ "s."
                where phrase    = expressSingle (fst xs)
                               ++ " "
                               ++ show (snd xs)

expressList :: (Show a, Eq a) => [(Int, a)] -> [String]
expressList = map expressGroup

express :: (Show a, Eq a) => (a -> [(Int, a)]) -> a -> String
express f x = unwords $ expressList xs
              where xs = f x

expressInt :: Integer -> String
expressInt = express runlengthInt

expressChar :: Char -> String
expressChar = express runlengthChar

expressString :: String -> String
expressString s = unwords $ map expressChar s
