{-Copyright (C) 2015 Payton Quinn-}

import System.Environment
import System.Exit
import Data.List.Split

-- Main function
main = do
    [inputFile,outputFile] <- getArgs
    verifyExtension inputFile 
    convert inputFile outputFile

-- Verifies that file extension is supported
verifyExtension inputFile
    | dropWhile (/= '.') inputFile == ".java" = putStrLn "Converting from Java"
    | otherwise = exit
    
-- Main convert function, sends to language-specific convert function
convert inputFile outputFile
    | dropWhile (/= '.') inputFile == ".java" = convertFromJava inputFile outputFile
    
-- Convert from java to english
convertFromJava inputFile outputFile = do
    s <- readFile inputFile
    let linesOfFile = lines s
    mapM lineParse linesOfFile
    putStr ""
    
lineParse ('f':'o':'r':' ':rest) = do
    let array = splitOneOf " (),:" rest
    forProcess array

lineParse ('f':'o':'r':rest)  = do
    let array = splitOneOf " (),:" rest
    forProcess array
    
lineParse ('S':'y':'s':'t':'e':'m':rest)  = do
    let array = splitOneOf ".()" rest
    sysProcess array
    
lineParse (' ':' ':' ':' ':rest)  = do
    putStr "    "
    lineParse rest 
    
lineParse (' ':' ':' ':rest)  = do
    putStr "   "
    lineParse rest 
    
lineParse (' ':rest)  = do
    lineParse rest 

lineParse ('}':rest) = putStrLn "."
       
lineParse line = do
    putStrLn line
    
forProcess ["",t,name,"=",startVal,endVal,"",counter,"","{"] = do
    putStrLn ("for some " ++ name ++ " of type " ++ t ++ " between " ++ startVal ++ " and " ++ (drop ((length endVal)-1) endVal) ++ ":")
    
forProcess ["",t,name,"","",list,"","{"] = do
    putStrLn ("for each " ++ name ++ " of type " ++ t ++ " in " ++ list ++ ":")
    
forProcess a = do
    print a

sysProcess ["",dest,"println",object,";"] = do
    putStrLn ("print " ++ (dropWhile (== ' ') object) ++ " to its own line")
    
sysProcess ["",dest,verb,object,";"] = do
    putStrLn (verb ++ " " ++ (dropWhile (== ' ') object))

sysProcess a = print a

-- exits gracefully
exit = do
    putStrLn "That file extension is not supported at this time."
    exitFailure
