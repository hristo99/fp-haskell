import Data.Char(toLower)

checkIfElem :: Char -> [Char] -> Bool
checkIfElem _ [] = False
checkIfElem l (x:xs) | l == x = True
                     | otherwise = checkIfElem l xs

isVowel :: Char -> Bool
isVowel l | checkIfElem l ['a', 'o', 'u', 'i', 'e'] = True
          | checkIfElem l ['A', 'O', 'U', 'I', 'E'] = True
          | otherwise = False


isConsonant :: Char -> Bool
isConsonant l | not (isVowel l) && (checkIfElem l ['a'..'z'] || checkIfElem l ['A'..'Z']) = True
              | otherwise = False


encode :: String -> String
encode [] = []
encode (x:xs) | isConsonant x = x : 'o' : toLower x : encode xs
              | otherwise = x : encode xs

dropN :: Int -> String -> String
dropN 0 str = str
dropN _ [] = []
dropN n (x:xs) = dropN (n - 1) xs

decode :: String -> String
decode [] = []
decode (x:xs) | isConsonant x = x : decode (dropN 2 xs)
              | otherwise = x : decode xs
