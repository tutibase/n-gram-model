module Main where

import Lib (parseText, cleanSentences, readLines, createDictionary, dictToString, parseDictFile, interactWithUser, startDialogue)
import Text.Parsec (ParseError)

main :: IO ()
main = do
    input <- readFile "input2.txt"
    case parseText input of
        Left err -> print (err :: ParseError)
        Right sentences ->  writeFile "preprocessed.txt" (unlines $ cleanSentences sentences)

    putStrLn "\nend of part 1.\n\n"

    {- 
    dictlines <- readLines "preprocessed.txt"
    let dict = createDictionary dictlines
    writeFile "dict.txt" (dictToString dict)
    putStrLn "\nend of part 2.\n\n"
    

    dictionary <- parseDictFile "dict.txt"
    interactWithUser dictionary
    putStrLn "\nend of part 3.\n\n"
    -}

    dictionary <- parseDictFile "dict.txt"
    startDialogue dictionary dictionary
    putStrLn "\nend of part 4.\n\n"