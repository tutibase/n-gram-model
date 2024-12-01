module Main where

import Lib (parseText, cleanSentences, readLines, createDictionary, dictToString, parseDictFile, interactWithUser)
import Text.Parsec (ParseError)

main :: IO ()
main = do
    input <- readFile "input2.txt"
    case parseText input of
        Left err -> print (err :: ParseError)
        Right sentences ->  writeFile "preprocessed.txt" (unlines $ cleanSentences sentences)

    putStrLn "\nend of part 1.\n\n"

    
    lines <- readLines "preprocessed.txt"
    let dict = createDictionary lines
    writeFile "dict.txt" (dictToString dict)
    putStrLn "\nend of part 2.\n\n"
    

    dict <- parseDictFile "dict.txt"
    interactWithUser dict
    putStrLn "\nend of part 3.\n\n"