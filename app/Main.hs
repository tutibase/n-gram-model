{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
module Main where

import Lib (parseText, cleanSentences, readLines, createDictionary, dictToString, parseDictFile, interactWithUser, startDialogue)
import Text.Parsec (ParseError)

main :: IO ()
main =
    putStrLn "\nChoose an option:" >>
    putStrLn "1. Text file preprocessing" >>
    putStrLn "2. Create N-gram model" >>
    putStrLn "3. Interact with model" >>
    putStrLn "4. Models dialogue" >>
    putStrLn "5. Exit" >>
    getLine >>= \choice ->
    case choice of
        "1" ->
            putStrLn "Enter input file name:" >>
            getLine >>= \inputFileName ->
            putStrLn "Enter output file name:" >>
            getLine >>= \outputFileName ->
            part1 inputFileName outputFileName >>
            main
        "2" ->
            putStrLn "Enter preprocessed file name:" >>
            getLine >>= \preprocessedFileName ->
            putStrLn "Enter dictionary file name:" >>
            getLine >>= \dictFileName ->
            part2 preprocessedFileName dictFileName >>
            main
        "3" ->
            putStrLn "Enter dictionary file name:" >>
            getLine >>= part3 >>
            main
        "4" ->
            putStrLn "Enter first dictionary file name:" >>
            getLine >>= \dictFileName1 ->
            putStrLn "Enter second dictionary file name:" >>
            getLine >>= \dictFileName2 ->
            part4 dictFileName1 dictFileName2 >>
            main
        "5" -> putStrLn "See you soon!"
        _   ->
            putStrLn "Invalid choice. Please try again." >>
            main

part1 :: String -> String -> IO ()
part1 inputFileName outputFileName =
    readFile inputFileName >>= \input ->
    case parseText input of
        Left err -> print (err :: ParseError)
        Right sentences -> writeFile outputFileName (unlines $ cleanSentences sentences) >>
                           putStrLn "\nCompleted successfully!\n"

part2 :: String -> String -> IO ()
part2 preprocessedFileName dictFileName =
    readLines preprocessedFileName >>= \dictlines ->
    writeFile dictFileName (dictToString $ createDictionary dictlines) >>
    putStrLn "\nCompleted successfully!\n"

part3 :: String -> IO ()
part3 dictFileName =
    parseDictFile dictFileName >>= \dictionary ->
    interactWithUser dictionary >>
    putStrLn "\nCompleted successfully!\n"

part4 :: String -> String -> IO ()
part4 dictFileName1 dictFileName2 =
    parseDictFile dictFileName1 >>= \dictionary1 ->
    parseDictFile dictFileName2 >>= \dictionary2 ->
    startDialogue dictionary1 dictionary2 >>
    putStrLn "\nCompleted successfully!\n"