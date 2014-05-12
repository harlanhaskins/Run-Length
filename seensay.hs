import Data.Digits
import Data.List

expressSingle :: Integer -> [Char]
expressSingle x = ns !! fromInteger x
                  where ns = ["zero", "one", "two", "three",
                              "four", "five", "six", "seven",
                              "eight", "nine"]

seensayList :: [Integer] -> [Integer]
seensayList xs = [toInteger (length xs), (xs !! 0)]

tokenize :: Integer -> [[Integer]]
tokenize = group . (digits 10)

seensay :: Integer -> Integer
seensay = (unDigits 10) . concat . mapSeensay
           where mapSeensay x = map seensayList $ tokenize x

seensayLevel :: Integer -> Integer -> Integer
seensayLevel 0 x = x
seensayLevel d x = seensayLevel (d - 1) $ seensay x

expressLevel :: Integer -> Integer -> [Char]
expressLevel d x = express $ seensayLevel d x

pairs :: [Integer] -> [(Integer, Integer)]
pairs []        = []
pairs (x:[])    = [(x,0)]
pairs (x:x':xs) = (x,x') : pairs xs

express :: Integer -> [Char]
express x = intercalate " " $ map expressGroup (pairs $ (digits 10) $ seensay x)

expressGroup :: (Integer, Integer) -> String
expressGroup xs | (fst xs == 1) = f ++ "."
                | otherwise     = f ++ "s."
                where f = expressSingle (fst xs) ++ " " ++ expressSingle (snd xs)
