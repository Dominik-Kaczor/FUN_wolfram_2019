--
-- EPITECH PROJECT, 2020
-- wolfram
-- File description:
-- wolfram
--

import System.Environment
import System.Exit
import Data.List
 
checkInt :: [Char] -> Bool
checkInt x
    | all(>='0') x == True && all (<='9') x == True = True
    | otherwise = False

checkNeg :: Char -> Bool
checkNeg x
    | x == '-' = True
    | otherwise = False

readInt  :: [Char] -> Maybe  Int
readInt [] = Nothing
readInt (a:b) = 
    if checkNeg a == True
        then if checkInt b == True
            then Just (read(a:b))
        else Nothing
    else if checkInt (a:b)
        then Just (read(a:b))
    else Nothing
 
getNextArgs :: [String] -> String
getNextArgs [] = "Empty"
getNextArgs (x:xs) = x
 
parseArgs :: [String] -> String -> String
parseArgs [] b = "Empty"
parseArgs (x:xs) b = case x == b of
    True -> getNextArgs xs
    False -> do
        parseArgs xs b
 
checkRule :: [String] -> IO String
checkRule args = do
    let rule = parseArgs args "--rule"
    let nb = readInt rule
    if (nb == Nothing) || (rule /= "30" && rule /= "90" && rule /= "110")
        then do
            putStrLn ("No rule set")
            exitWith (ExitFailure 84)
        else
            return rule

checkStart :: [String] -> IO Int
checkStart args = do
    let start = parseArgs args "--start"
    if start == "Empty"
        then return 0
    else do
        let nb = readInt start
        if (nb == Nothing)
            then do
                putStrLn ("Start is not an integer")
                exitWith (ExitFailure 84)
            else
                return (read start)
 
checkLines :: [String] -> IO Int
checkLines args = do
    let lines = parseArgs args "--lines"
    if lines == "Empty"
        then return (-1)
    else do
        let nb = readInt lines
        if (nb == Nothing)
            then do
                putStrLn ("Lines is not an integer")
                exitWith (ExitFailure 84)
            else
                return (read lines)
 
checkWindow :: [String] -> IO Int
checkWindow args = do
    let window = parseArgs args "--window"
    if window == "Empty"
        then return 80
    else do
        let nb = readInt window
        if (nb == Nothing)
            then do
                putStrLn ("Window is not an integer")
                exitWith (ExitFailure 84)
            else
                return (read window)
 
checkMove :: [String] -> IO Int
checkMove args = do
    let move = parseArgs args "--move"
    if move == "Empty"
        then return 0
    else do
        let nb = readInt move
        if (nb == Nothing)
            then do
                putStrLn ("Move is not an integer")
                exitWith (ExitFailure 84)
            else
                return (read move)

checkArgs :: [String] -> IO Int
checkArgs [] = return 84
checkArgs (args:as) =   if args == "--rule" || args == "--start" || args == "--lines" || args == "--window" || args == "--move"
                                then if readInt (head as) == Nothing
                                    then do
                                        putStrLn ("The arguments is not an integer")
                                        exitWith (ExitFailure 84)
                                else
                                    if length (args:as) == 2
                                        then return 0
                                    else
                                        checkArgs (tail as)
                        else do
                            putStrLn ("The arguments is not valid arguments")
                            exitWith (ExitFailure 84)

rule_30 :: [Char] -> [Char] -> Int -> [Char]
rule_30 (la:lb) new_list max =   if max >= 1
                                        then if la == '*' && head lb == '*' && head (tail lb) == '*'
                                            then rule_30 lb (new_list++[' ']) (max - 1)

                                        else if la == '*' && head lb == '*' && head (tail lb) == ' '
                                            then rule_30 lb (new_list++[' ']) (max - 1)

                                        else if la == '*' && head lb == ' ' && head (tail lb) == '*'
                                            then rule_30 lb (new_list++[' ']) (max - 1)

                                        else if la == '*' && head lb == ' ' && head (tail lb) == ' '
                                            then rule_30 lb (new_list++['*']) (max - 1)

                                        else if la == ' ' && head lb == '*' && head (tail lb) == '*'
                                            then rule_30 lb (new_list++['*']) (max - 1)

                                        else if la == ' ' && head lb == '*' && head (tail lb) == ' '
                                            then rule_30 lb (new_list++['*']) (max - 1)

                                        else if la == ' ' && head lb == ' ' && head (tail lb) == '*'
                                            then rule_30 lb (new_list++['*']) (max - 1)

                                        else rule_30 lb (new_list++[' ']) (max - 1)
                                    else new_list

rule_90 :: [Char] -> [Char] -> Int -> [Char]
rule_90 (la:lb) new_list max =  if max >= 1
                                    then if la == '*' && head lb == '*' && head (tail lb) == '*' --111
                                        then rule_90 lb (new_list++[' ']) (max - 1) --0

                                    else if la == '*' && head lb == '*' && head (tail lb) == ' ' --110
                                        then rule_90 lb (new_list++['*']) (max - 1) --1

                                    else if la == '*' && head lb == ' ' && head (tail lb) == '*' --101
                                        then rule_90 lb (new_list++[' ']) (max - 1) --0

                                    else if la == '*' && head lb == ' ' && head (tail lb) == ' ' --100
                                        then rule_90 lb (new_list++['*']) (max - 1) --1

                                    else if la == ' ' && head lb == '*' && head (tail lb) == '*' --011
                                        then rule_90 lb (new_list++['*']) (max - 1) --1

                                    else if la == ' ' && head lb == '*' && head (tail lb) == ' ' --010
                                        then rule_90 lb (new_list++[' ']) (max - 1) --0

                                    else if la == ' ' && head lb == ' ' && head (tail lb) == '*' --001
                                        then rule_90 lb (new_list++['*']) (max - 1) --1

                                    else rule_90 lb (new_list++[' ']) (max - 1) -- 0
                                else new_list

rule_110 :: [Char] -> [Char] -> Int -> [Char]
rule_110 (la:lb) new_list max =  if max >= 1
                                then if la == '*' && head lb == '*' &&  head (tail lb) == '*'
                                    then rule_110 lb (new_list++[' ']) (max - 1)

                                else if la == '*' && head lb == '*' && head (tail lb) == ' '
                                    then rule_110 lb (new_list++['*']) (max - 1)

                                else if la == '*' && head lb == ' ' && head (tail lb) == '*'
                                    then rule_110 lb (new_list++['*']) (max - 1)

                                else if la == '*' && head lb == ' ' && head (tail lb) == ' '
                                    then rule_110 lb (new_list++[' ']) (max - 1)

                                else if la == ' ' && head lb == '*' && head (tail lb) == '*'
                                    then rule_110 lb (new_list++['*']) (max - 1)

                                else if la == ' ' && head lb == '*' && head (tail lb) == ' '
                                    then rule_110 lb (new_list++['*']) (max - 1)

                                else if la == ' ' && head lb == ' ' && head (tail lb) == '*'
                                    then rule_110 lb (new_list++['*']) (max - 1)

                                else rule_110 lb (new_list++[' ']) (max - 1)
                            else new_list

printSpaces :: Int -> IO Int
printSpaces x = if x > 1
                    then do
                        putStr(" ")
                        printSpaces (x-1)
                else return 0

printIndexList :: [Char] -> Int -> Int -> IO Int
printIndexList list start nb =  if start == 0
                                    then if (nb == 0) 
                                        then return 0
                                    else do
                                        putChar (head list)
                                        printIndexList (tail list) start (nb - 1)
                                else printIndexList (tail list) (start - 1) nb

displayRule :: [Char] -> Int -> Int -> Int -> String -> IO Int
displayRule list move window len rule =  if rule == "110"
                                            then if len <= ((window `div` 2) + 3)
                                                then do
                                                    printSpaces ((window `div` 2) - (len - 4))
                                                    putStr list
                                                    printSpaces (window `div` 2)
                                                    putChar '\n'
                                                    return 0
                                            else do
                                                printIndexList list (len - ((window `div` 2) + 3)) ((window `div` 2) + 1)
                                                printSpaces (window `div` 2)
                                                putChar '\n'
                                                return 0
                                        else
                                            if len <= (window + 1)
                                            then do
                                                printSpaces (((window `div` 2) + 1) - ((len - 2) `div` 2) + move)
                                                putStr list
                                                printSpaces ((window `div` 2) - ((len - 2) `div` 2) - move)
                                                putChar '\n'
                                                return 0
                                            else do
                                                printIndexList list (((len - window) `div` 2) - 1) window
                                                putChar '\n'
                                                return 0

managment_rule :: String -> [Char] -> [Char]
managment_rule rule list =  if rule == "30"
                                then (rule_30 list [' ', '*'] ((length list) - 2)) ++ ['*', ' ']
                            else if rule == "90"
                                then (rule_90 list [' ', '*'] ((length list) - 2)) ++ ['*', ' ']
                            else
                                (rule_110 list [' ', '*'] ((length list) - 2)) ++ [' ']

core :: String -> Int -> Int -> Int -> Int -> [Char] -> IO Int
core rule start lines window move list =    if lines /= 0
                                                then if start <= 0
                                                    then do
                                                        displayRule (init (tail list)) move window (length list) rule
                                                        core rule (start - 1) (lines - 1) window move (managment_rule rule list)
                                                else core rule (start - 1) lines window move (managment_rule rule list)
                                            else
                                                return 0
main = do
    args <- getArgs
    checkArgs args
    move <- checkMove args
    rule <- checkRule args
    lines <- checkLines args
    window <- checkWindow args
    start <- checkStart args
    core rule start lines window move [' ','*',' ']
    return 0