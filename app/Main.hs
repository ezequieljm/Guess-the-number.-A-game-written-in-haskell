module Main where

import System.Random (Random (randomR), mkStdGen, StdGen)
import System.IO (hFlush, stdout)
import Data.Maybe (isNothing)
import Text.Read (readMaybe)
import System.Process (system)
import GHC.IO.Exception (ExitCode)
import System.Info (os)

type OptionMenu = (String, String)
type RandomInterval a = (a,StdGen)
type ValueOrdering = (Int, Ordering)

data Interval = Initial | Final

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


mainMenu :: String -> [OptionMenu] -> IO String
mainMenu text xs = do 
    clear
    putStrLn "<< GUESS THE NUMBER. A game written in Haskell >>"
    mapM_ (putStrLn . fst) xs
    option  <- prompt text
    if option `elem` mainOptionList then 
        return option
    else 
        mainMenu "Incorrect Option. Please re-enter: " xs
    where 
        mainOptionList = map snd xs


newGame :: String -> IO ()
newGame text = do 
    clear 
    mapM_ (putStrLn . fst) optionNewGame
    opt <- prompt text
    if opt == "3" then 
        return ()
    else
        if opt `elem` map snd optionNewGame then do
            clear
            showMenuGame opt
            putStrLn "Continue press <ENTER>..."
            _ <- getLine
            newGame "Option: "
        else
            newGame "Incorrect Option. Please re-enter: "
    where 
        showMenuGame :: String -> IO ()
        showMenuGame text = case text of 
                                "1" -> playerVsCpu
                                "2" -> playerVsPlayer


showRanking :: IO ()
showRanking = do 
    clear 
    putStrLn "Ranking of players\nContinue press <ENTER>"
    _ <- getLine
    return ()


switchGame :: String -> IO ()
switchGame "1" = newGame "Option: "
switchGame "2" = showRanking


main :: IO ()
main = do 
    option  <- mainMenu "Choose an option: " optionMainMenu
    if option == "3" then 
        putStrLn "End Game. "
    else do
        switchGame option 
        main


playerVsCpu :: IO ()
playerVsCpu = do 
    initial <- controlIntegers Initial "Initial: "
    final   <- controlIntegers Final "Final: "
    let correctInterval = if initial >= final then (final, initial) else (initial, final)
    randomInterval  <- createSeedValue correctInterval
    runGamePvC randomInterval correctInterval


createSeedValue :: (Int, Int) -> IO (RandomInterval Int)
createSeedValue interval = return $ randomR interval (mkStdGen $ fst interval)


getAndControlInput :: Interval -> String -> IO Int
getAndControlInput intervalExt txt = do
    inputUser <- prompt txt
    let valueInt = readMaybe inputUser :: Maybe Int
        in  if isNothing valueInt then
                controlIntegers intervalExt "Incorrect Value. Please re-enter: "
            else
                return $ read inputUser


controlIntegers :: Interval -> String -> IO Int
controlIntegers Initial txt = do
    clear
    mapM_ putStrLn ["<<< PLAYER VS CPU >>", "Enter the initial value"]
    getAndControlInput Initial txt
controlIntegers Final txt = do
    clear
    mapM_ putStrLn ["<<< PLAYER VS CPU >>", "Enter the final value"]
    getAndControlInput Final txt


runGamePvC :: RandomInterval Int -> (Int, Int) -> IO ()
runGamePvC randomInterval initialInteraval = do
    print $ "Initial Interval: " ++ show initialInteraval
    print $ "Random Interval: " ++ show randomInterval


getNewInterval :: [ValueOrdering] -> IO (Int, Int)
getNewInterval xs = 
    let smallers = filter (\(x, y) -> y == LT)
        greatters = filter (\(x, y) -> y == GT)
        selectSmaller (x, y) (w, z) = if x >= w then (x, y) else (w, z)
        selectGreatter (x, y) (w, z) = if x >= w then (w, z) else (x, y)
        newInitial  = fst $ (foldl1 $! selectSmaller) (smallers xs)
        newFinal    = fst $ (foldl1 $! selectGreatter) (greatters xs)
        in return (newInitial, newFinal)


playerVsPlayer :: IO ()
playerVsPlayer = putStrLn "Player vs Player"