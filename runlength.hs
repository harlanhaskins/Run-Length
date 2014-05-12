import Data.Digits
import Data.List

expressSingle :: Integer -> [Char]
expressSingle x = ns !! fromInteger x
                  where ns = ["zero", "one", "two", "three",
                              "four", "five", "six", "seven",
                              "eight", "nine"]

runlengthList :: [Integer] -> [Integer]
runlengthList xs = [toInteger (length xs), (xs !! 0)]

tokenize :: Integer -> [[Integer]]
tokenize = group . (digits 10)

runlength :: Integer -> Integer
runlength = (unDigits 10) . concat . maprunlength
           where maprunlength x = map runlengthList $ tokenize x

runlengthLevel :: Integer -> Integer -> Integer
runlengthLevel 0 x = x
runlengthLevel d x = runlengthLevel (d - 1) $ runlength x

expressLevel :: Integer -> Integer -> [Char]
expressLevel d x = express $ runlengthLevel d x

pairs :: [Integer] -> [(Integer, Integer)]
pairs []        = []
pairs (x:[])    = [(x,0)]
pairs (x:x':xs) = (x,x') : pairs xs

express :: Integer -> [Char]
express x = intercalate " " $ map expressGroup (pairs $ (digits 10) $ runlength x)

expressGroup :: (Integer, Integer) -> String
expressGroup xs | (fst xs == 1) = f ++ "."
                | otherwise     = f ++ "s."
                where f =  expressSingle (fst xs)
                        ++ " "
                        ++ expressSingle (snd xs)
