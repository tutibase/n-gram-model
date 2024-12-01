module Main where

import Lib (parseText, cleanSentences, readLines, createDictionary, dictToString)
import System.IO (readFile, writeFile)
import Text.Parsec (ParseError)

main :: IO ()
main = do
    input <- readFile "input.txt"
    case parseText input of
        Left err -> print (err :: ParseError)
        Right sentences ->  writeFile "preprocessed.txt" (unlines $ cleanSentences sentences)

    putStrLn "\nend of part 1.\n\n"

    
    lines <- readLines "preprocessed.txt"
    let dict = createDictionary lines
    writeFile "dict.txt" (dictToString dict)
    putStrLn "\nend of part 2.\n\n"
    
