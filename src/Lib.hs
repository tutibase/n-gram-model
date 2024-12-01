module Lib
    ( parseText, 
    cleanSentences,
    readLines,
    createDictionary,
    dictToString
    ) where

import Text.Parsec
import Text.Parsec.String (Parser)
import Data.Char (isAlpha)
import Data.List (nub)
import Data.Map (Map, fromListWith, toList, keys, insert)
import Text.Printf (printf)
-- import qualified Data.Text as T

-- 1 часть: парсинг текста предложения
-- Парсер для предложения
sentence :: Parser String
sentence = do
    content <- many (noneOf ".!?:;()")
    end <- oneOf ".!?:;()"
    return (content ++ [end])

-- Парсер для текста
text :: Parser [String]
text = many1 (sentence <* spaces)

-- Функция для парсинга текста
parseText :: String -> Either ParseError [String]
parseText = parse text ""

-- Функция для очистки предложений от цифр и пунктуации
cleanSentences :: [String] -> [String]
cleanSentences = map cleanSentence
  where
    cleanSentence :: String -> String
    cleanSentence = unwords. words . unwords . map cleanWord . words

    cleanWord :: String -> String
    cleanWord = map (\c -> if isAlpha c then c else ' ')


-- 2 часть: N-граммы

-- Функция для чтения файла и разбиения его на строки
readLines :: FilePath -> IO [String]
readLines filePath = do
    content <- readFile filePath
    return (lines content)

-- Функция для создания биграмм и триграмм из строки
createNGrams :: String -> [(String, String)]
createNGrams line =
    let wordList = words line
        bigrams = zip wordList (drop 1 wordList)
        trigrams = zip (zip wordList (drop 1 wordList)) (drop 2 wordList)
        singleWordPairs = zip wordList (zip (drop 1 wordList) (drop 2 wordList))
    in bigrams ++ map (\((w1, w2), w3) -> (w1 ++ " " ++ w2, w3)) trigrams ++ map (\(w1, (w2, w3)) -> (w1, w2 ++ " " ++ w3)) singleWordPairs

-- Функция для создания словаря из списка строк
createDictionary :: [String] -> Map String [String]
createDictionary lines =
    let allNGrams = concatMap createNGrams lines
        allWords = concatMap words lines
        allPairs = concatMap (\line -> zip (words line) (drop 1 (words line))) lines
        allKeys = nub (allWords ++ map (\(w1, w2) -> w1 ++ " " ++ w2) allPairs)
        groupedNGrams = fromListWith (++) [(key, [value]) | (key, value) <- allNGrams]
        completeDict = foldr (\key acc -> if key `elem` keys groupedNGrams then acc else insert key [] acc) groupedNGrams allKeys
        uniqueValuesDict = fmap nub completeDict
    in uniqueValuesDict


-- Функция для преобразования словаря в строку для записи в файл
dictToString :: Map String [String] -> String
dictToString dict =
    let dictList = toList dict
        formatEntry (key, values) = printf "\"%s\" : %s" key (show values)
    in unlines (map formatEntry dictList)

{-
-- Функция для преобразования словаря в строку для записи в файл
dictToString :: Map String [String] -> String
dictToString dict =
    let dictList = toList dict
        formatEntry (key, values) = printf "\"%s\" : [%s]" key (showValues values)
        showValues = T.unpack . T.concat . map (\value -> T.concat [T.pack "\"", T.pack value, T.pack "\", "]) . nub
    in unlines (map formatEntry dictList)
-}
