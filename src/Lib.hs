module Lib
    ( parseText,
    cleanSentences,
    readLines,
    createDictionary,
    dictToString,
    parseDictFile,
    interactWithUser,
    startDialogue
    ) where

import Text.Parsec
import Text.Parsec.String (Parser)
import Data.Char (isAlpha)
import Data.List (nub)
import Data.Map (Map, fromListWith, toList, keys, insert, fromList, (!?))
import Text.Printf (printf)
import System.Random ( randomRIO )
import Control.Monad (when)


-- 1 часть: парсинг текста предложения
-- Парсер для предложения
sentence :: Parser String
sentence = many (noneOf ".!?:;()") >>= \content ->
           oneOf ".!?:;()" >>= \end ->
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
    cleanWord = map (\c -> if isAlpha c || c == '\'' then c else ' ')



-- 2 часть: N-граммы

-- Функция для чтения файла и разбиения его на строки
readLines :: FilePath -> IO [String]
readLines filePath = lines <$> readFile filePath

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
createDictionary dictlines =
    let allNGrams = concatMap createNGrams dictlines
        allWords = concatMap words dictlines
        allPairs = concatMap (\line -> zip (words line) (drop 1 (words line))) dictlines
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




-- 3.1 часть: парсинг файла в Map

--- Парсер для строки
parseString :: Parser String
parseString = char '"' >> many (noneOf "\"") >>= \content -> char '"' >> return content

-- Парсер для списка строк
parseStringList :: Parser [String]
parseStringList = char '[' >> parseString `sepBy` (char ',' >> spaces) >>= \strings -> char ']' >> return strings

-- Парсер для одной записи в словаре
parseDictEntry :: Parser (String, [String])
parseDictEntry = parseString >>= \key -> string " : " >> parseStringList >>= \value -> return (key, value)

-- Парсер для всего файла
parseDict :: Parser (Map String [String])
parseDict = parseDictEntry `endBy` newline >>= \entries -> return (fromList entries)

-- Функция для парсинга файла и получения Map
parseDictFile :: FilePath -> IO (Map String [String])
parseDictFile filePath =
    readLines filePath >>= (\input ->
    case parse parseDict "" input of
        Left err -> error (show err)
        Right result -> return result) . unlines


-- 3.2 часть: взаимодействие с пользователем

-- Функция для генерации случайного элемента из списка
randomElement :: [a] -> IO a
randomElement xs = randomRIO (0, length xs - 1) >>= \index -> return (xs !! index)

-- Функция для генерации фразы
generatePhrase :: Map String [String] -> String -> IO String
generatePhrase dict startWords =
    generatePhraseHelper dict [startWords] (length (words startWords))

-- Вспомогательная функция для генерации фразы
generatePhraseHelper :: Map String [String] -> [String] -> Int -> IO String
generatePhraseHelper dict phrase currentLength =
    case dict !? last phrase of
        Nothing -> return (unwords phrase)
        Just nextWordsList ->
            if currentLength >= 15 || null nextWordsList then
                return (unwords phrase)
            else
                randomElement nextWordsList >>= \nextWords ->
                generatePhraseHelper dict (phrase ++ [nextWords]) (currentLength + length (words nextWords))

-- Функция для взаимодействия с пользователем
interactWithUser :: Map String [String] -> IO ()
interactWithUser dict =
    putStrLn "Введите одно слово или пару слов (или 'exit' для выхода):" >>
    getLine >>= \input ->
    when (input /= "exit") $
        case dict !? input of
            Nothing -> putStrLn "Заданное слово или пара слов не найдены в словаре." >> interactWithUser dict
            Just _ -> generatePhrase dict input >>= \phrase ->
                putStrLn ("Сгенерированная фраза: " ++ phrase) >> interactWithUser dict


-- 4 часть: 2 модели

-- Функция для поиска подходящего слова в предложении оппонента
findSuitableWord :: Map String [String] -> [String] -> Maybe String
findSuitableWord dict response = go (reverse response)
  where
    go [] = Nothing
    go (w:ws) = case dict !? w of
        Nothing -> go ws
        Just nextWordsList -> if null nextWordsList then go ws else Just w

-- Функция для взаимодействия двух моделей
interactModels :: Map String [String] -> Map String [String] -> String -> Int -> IO ()
interactModels dict1 dict2 startWords depth =
    when (depth > 0) $
        generatePhrase dict1 startWords >>= \phrase1 ->
        putStrLn ("Модель 1: " ++ phrase1) >>
        if depth - 1 == 0 then
            return ()
        else
            let responseWords1 = words phrase1
            in case findSuitableWord dict2 responseWords1 of
                Nothing -> putStrLn "Модель 2 не может ответить."
                Just response1 ->
                    generatePhrase dict2 response1 >>= \phrase2 ->
                    putStrLn ("Модель 2: " ++ phrase2) >>
                    let responseWords2 = words phrase2
                    in case findSuitableWord dict1 responseWords2 of
                        Nothing -> putStrLn "Модель 1 не может ответить."
                        Just response2 -> interactModels dict1 dict2 response2 (depth - 2)

-- Функция для запуска диалога
startDialogue :: Map String [String] -> Map String [String] -> IO ()
startDialogue dict1 dict2 = do
    putStrLn "Введите начальное слово (или пару слов):"
    startWords <- getLine
    putStrLn "Введите глубину M сообщений:"
    depth <- readLn
    interactModels dict1 dict2 startWords depth