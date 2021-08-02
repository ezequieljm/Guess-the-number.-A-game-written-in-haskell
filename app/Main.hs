module Main where

import System.Random (Random (randomR), mkStdGen)
import System.IO (hFlush, stdout)
import Data.Maybe (isNothing)
import Text.Read (readMaybe, Lexeme (String))
import System.Process (system)
import GHC.IO.Exception (ExitCode)
import System.Info (os)

type OptionMenu = (String, String)
optionMainMenu :: [OptionMenu]
optionMainMenu = 
    [ ("[1] New Game", "1")
    , ("[2] Ranking", "2")
    , ("[3] Exit Game", "3")
    ]

optionNewGame :: [OptionMenu]
optionNewGame = 
    [ ("[1] Player vs CPU", "1")
    , ("[2] Player vs Player", "2")
    , ("[3] <- Go Back", "3")
    ]


clear :: IO ExitCode
clear = if os == "linux" then system "clear" else system "cls"


prompt :: String -> IO String
prompt text = do 
    putStr text
    hFlush stdout
    getLine


controlIntegers :: String -> IO Int
controlIntegers txt = do
    lineUser    <- prompt txt
    if isNothing (readMaybe lineUser :: Maybe Int) 
        then controlIntegers "The value isn't a number. Please re-enter: "
        else return $ read lineUser


mainMenu :: String -> [OptionMenu] -> IO String
mainMenu text xs = do 
    clear
    putStrLn "<< GUESS THE NUMBER. A game written in Haskell >>"
    mapM_ (putStrLn . fst) xs
    option  <- prompt text
    if option `elem` map snd xs 
        then return option
        else mainMenu "Incorrect Option. Please re-enter: " xs


switchGame :: String -> IO ()
switchGame optionUser
    | optionUser == "1"     = newGame "Option: "
    | optionUser == "2"     = showRanking
    where 
        newGame :: String -> IO ()
        newGame text = do 
            clear 
            mapM_ (putStrLn . fst) optionNewGame
            opt   <- prompt text
            if opt == "3" then 
                return ()
            else 
                if opt `elem` map snd optionNewGame then do
                    clear
                    putStrLn $ 
                        case opt of 
                            "1"     -> "Player vs CPU"
                            "2"     -> "Player vs Player"
                        ++ "\n" ++ "Continue press <ENTER>"
                else
                    newGame "Incorrect Option. Please re-enter: "
        showRanking :: IO ()
        showRanking = do 
            clear 
            putStrLn "Ranking of players\nContinue press <ENTER>"

main :: IO ()
main = do 
    option  <- mainMenu "Choose an option: " optionMainMenu
    if option == "3" then 
        putStrLn "End Game. "
    else do
        switchGame option 
        main

