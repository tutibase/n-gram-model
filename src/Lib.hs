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
    cleanWord = map (\c -> if isAlpha c || c == '\'' then c else ' ')



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
parseDictFile filePath = do
    content <- readLines filePath
    let input = unlines content
    case parse parseDict "" input of
        Left err -> error (show err)
        Right result -> return result


-- 3.2 часть: взаимодействие с пользователем

-- Функция для генерации случайного элемента из списка
randomElement :: [a] -> IO a
randomElement xs = do
    index <- randomRIO (0, length xs - 1)
    return (xs !! index)

-- Функция для генерации фразы
generatePhrase :: Map String [String] -> String -> IO String
generatePhrase dict startWords = do
    let currentWords = startWords
    let phraseLength = length (words currentWords)
    generatePhraseHelper dict [currentWords] phraseLength

-- Вспомогательная функция для генерации фразы
generatePhraseHelper :: Map String [String] -> [String] -> Int -> IO String
generatePhraseHelper dict phrase currentLength = do
    let lastWords = last phrase
    case dict !? lastWords of
        Nothing -> return (unwords phrase)
        Just nextWordsList -> do
            if currentLength >= 15 || null nextWordsList then
                return (unwords phrase)
            else do
                nextWords <- randomElement nextWordsList
                let newPhrase = phrase ++ [nextWords]
                let newLength = currentLength + length (words nextWords)
                generatePhraseHelper dict newPhrase newLength

-- Функция для взаимодействия с пользователем
interactWithUser :: Map String [String] -> IO ()
interactWithUser dict = do
    putStrLn "Введите одно слово или пару слов (или 'exit' для выхода):"
    input <- getLine
    when (input /= "exit") $ do
        case dict !? input of
            Nothing -> putStrLn "Заданное слово или пара слов не найдены в словаре."
            Just _ -> do
                phrase <- generatePhrase dict input
                putStrLn ("Сгенерированная фраза: " ++ phrase)
        interactWithUser dict


-- 4 часть: 2 модели

-- Функция для поиска подходящего слова в предложении оппонента
findSuitableWord :: Map String [String] -> [String] -> Maybe String
findSuitableWord dict words = go (reverse words)
  where
    go [] = Nothing
    go (w:ws) = case dict !? w of
        Nothing -> go ws
        Just nextWordsList -> if null nextWordsList then go ws else Just w

-- Функция для взаимодействия двух моделей
interactModels :: Map String [String] -> Map String [String] -> String -> Int -> IO ()
interactModels dict1 dict2 startWords depth = do
    when (depth > 0) $ do
        phrase1 <- generatePhrase dict1 startWords
        putStrLn ("Модель 1: " ++ phrase1)
        if depth - 1 == 0 then
            return ()
        else do
            let responseWords1 = words phrase1
            case findSuitableWord dict2 responseWords1 of
                Nothing -> putStrLn "Модель 2 не может ответить."
                Just response1 -> do
                    phrase2 <- generatePhrase dict2 response1
                    putStrLn ("Модель 2: " ++ phrase2)
                    let responseWords2 = words phrase2
                    case findSuitableWord dict1 responseWords2 of
                        Nothing -> putStrLn "Модель 1 не может ответить."
                        Just response2 -> interactModels dict1 dict2 response2 (depth - 2)

-- Функция для запуска диалога
startDialogue :: Map String [String] -> Map String [String] -> IO ()
startDialogue dict1 dict2 = do
    putStrLn "Введите начальное слово (или пару слов) и глубину M сообщений:"
    input <- getLine
    let (startWords, depthStr) = span (/= ' ') input
    let depth = read (dropWhile (== ' ') depthStr) :: Int
    interactModels dict1 dict2 startWords depth