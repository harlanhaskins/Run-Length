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

-- Expression (turning run-length encoded input into 
-- human-readable Strings)

expressSingle :: Int -> String
expressSingle x | x < 10    = ns !! x
                | otherwise = show x
                where ns = ["zero", "one", "two", "three",
                            "four", "five", "six", "seven",
                            "eight", "nine"]

expressGroup :: (Show a, Eq a) => (Int, a) -> String
expressGroup (count, noun) | count == 1 = phrase ++ "."
                           | otherwise  = phrase ++ "s."
                       where
                            phrase = expressSingle count
                                  ++ " "
                                  ++ show noun

expressList :: (Show a, Eq a) => [(Int, a)] -> [String]
expressList = map expressGroup

express :: (Show a, Eq a) => (a -> [(Int, a)]) -> a -> String
express f x = unwords $ expressList xs
              where xs = f x

expressInt :: Integer -> String
expressInt = express runlengthInt

expressChar :: Char -> String
expressChar = express runlengthChar

-- Kludgey, but works. Wishing I could re-use express more explicitly.
expressString :: String -> String
expressString s = unwords $ map expressChar s
