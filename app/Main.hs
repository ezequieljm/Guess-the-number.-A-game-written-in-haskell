module Main where

import System.Random (Random (randomR), mkStdGen, StdGen)
import System.IO (hFlush, stdout)
import Data.Maybe (isNothing)
import Text.Read (readMaybe)
import System.Process (system)
import GHC.IO.Exception (ExitCode)
import System.Info (os)

---------------------------------------------------------------------------------------------------
-- Definitions datas and types
---------------------------------------------------------------------------------------------------

type OptionMenu = (String, String)
type RandomInterval a = (a,StdGen)
type ValueOrdering = (Int, Ordering)

data Interval = Initial | Final
data Player = Person | Cpu deriving (Show, Read)

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


---------------------------------------------------------------------------------------------------
--  Auxiliar functions
---------------------------------------------------------------------------------------------------

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

---------------------------------------------------------------------------------------------------
--  PLAYER VS CPU
---------------------------------------------------------------------------------------------------

-- Auxiliar functions

createSeedValue :: (Int, Int) -> IO (RandomInterval Int)
createSeedValue interval = return $ randomR interval (mkStdGen $ fst interval)

getInteger :: String -> IO Int
getInteger text = do
    line    <- prompt text
    let valueMaybeInt = readMaybe line :: Maybe Int
        in  if isNothing valueMaybeInt then do
                clear
                getInteger $ (head . lines) text ++ " \nThe value isn't a interger. Please re-enter: "
            else
                return $ read line

getAndControlInput :: Interval -> String -> IO Int
getAndControlInput intervalExt txt = do
    inputUser <- prompt txt
    let valueInt = readMaybe inputUser :: Maybe Int
        in  if isNothing valueInt then
                controlIntegersWithInterval intervalExt "Incorrect Value. Please re-enter: "
            else
                return $ read inputUser

controlIntegersWithInterval :: Interval -> String -> IO Int
controlIntegersWithInterval Initial txt = do
    clear
    mapM_ putStrLn ["<<< PLAYER VS CPU >>", "Enter the initial value"]
    getAndControlInput Initial txt
controlIntegersWithInterval Final txt = do
    clear
    mapM_ putStrLn ["<<< PLAYER VS CPU >>", "Enter the final value"]
    getAndControlInput Final txt

getNewInterval :: [ValueOrdering] -> IO (Int, Int)
getNewInterval xs = 
    let smallers = filter (\(x, y) -> y == LT)
        greatters = filter (\(x, y) -> y == GT)
        selectSmaller (x, y) (w, z) = if x >= w then (x, y) else (w, z)
        selectGreatter (x, y) (w, z) = if x >= w then (w, z) else (x, y)
        newInitial  = fst $ (foldl1 $! selectSmaller) (smallers xs)
        newFinal    = fst $ (foldl1 $! selectGreatter) (greatters xs)
        in return (newInitial, newFinal)

-- Game functions

-- Function get winner of game: The winners can are a person or cpu
getWinnerPvC :: [ValueOrdering] -> Player -> StdGen -> Int -> IO Player
-- Case turn Person
getWinnerPvC xs Person stdGenNext seedValue = do
    clear
    valueUser <- getInteger $ "Values entered: " ++ show xs ++ "\nWhat number am i thinking?: "
    let compareValueSeed = compare valueUser seedValue
        lessThan    = (valueUser, LT):xs
        greaterThan = (valueUser, GT):xs
        in  if compareValueSeed == EQ then
                return Person
            else do
                clear
                putStrLn $ "Values entered: " ++ show xs
                case compareValueSeed of 
                    LT -> do 
                        putStrLn "Sorry the value is BIGGER. Turn CPU press ENTER..."
                        _ <- getLine
                        getWinnerPvC lessThan Cpu stdGenNext seedValue
                    GT -> do
                        putStrLn "Sorry the value is SMALLER. Turn CPU press ENTER..."
                        _ <- getLine
                        getWinnerPvC greaterThan Cpu stdGenNext seedValue
-- In case of turn Cpu
getWinnerPvC xs Cpu stdGenNext seedValue = do
    interval <- getNewInterval xs
    let (valueCpu, newStdGen) = randomR interval stdGenNext :: (Int, StdGen)
        compareSeedValue = compare valueCpu seedValue
        lessThan = (valueCpu, LT):xs
        greaterThan = (valueCpu, GT):xs
        in if compareSeedValue == EQ then 
                return Cpu
            else do 
                clear
                putStrLn $ "Values entered: " ++ show xs
                putStrLn $ "Cpu said: " ++ show valueCpu ++ "\nPress Enter to continue..."
                _ <- getLine
                case compareSeedValue of 
                    LT -> getWinnerPvC lessThan Person stdGenNext seedValue
                    GT -> getWinnerPvC greaterThan Person stdGenNext seedValue
                                                
runGamePvC :: RandomInterval Int -> (Int, Int) -> IO ()
runGamePvC randomInterval initialInteraval = do
    let listValueOrdered = [(fst initialInteraval, LT),(snd initialInteraval, GT)]
        seedValue = fst randomInterval
        rootStdGen = snd randomInterval
    winner <- getWinnerPvC listValueOrdered Person rootStdGen seedValue
    clear
    case winner of
        Person  -> putStrLn $ "Congratulations you Win!!! The number was " ++ show seedValue 
        Cpu     -> putStrLn $ "Cpu: I won. Saver defeat!!! The number was " ++ show seedValue

-- Main game mode player vs cpu
playerVsCpu :: IO ()
playerVsCpu = do 
    initial <- controlIntegersWithInterval Initial "Initial: "
    final   <- controlIntegersWithInterval Final "Final: "
    let correctInterval = if initial >= final then (final, initial) else (initial, final)
        in do 
            randomInterval  <- createSeedValue correctInterval
            runGamePvC randomInterval correctInterval


---------------------------------------------------------------------------------------------------
--  PLAYER VS PLAYER
---------------------------------------------------------------------------------------------------

controlInput :: String -> String -> IO Int
controlInput msg str = 
    if isNothing (readMaybe str :: Maybe Int) then do
        clear
        putStrLn msg
        input <- prompt "The value isn't an integer. Please re-enter: "
        controlInput msg input
    else
        return $ read str


getExtremesOfTheInterval :: IO (Int, Int)
getExtremesOfTheInterval = do
    clear 
    -- We get the first extreme of the interval
    firstExt            <- prompt "<<< PLAYER VS PLAYER >>>\nEnter the initial extreme\nInitial: "
    correctFirstExt     <- controlInput "<<< PLAYER VS PLAYER >>>\nEnter the initial extreme" firstExt
    -- We get the second extreme of the interval
    clear
    secondExt           <- prompt "<<< PLAYER VS PLAYER >>>\nEnter the final extreme\nFinal: "
    correctSecondExt    <- controlInput "<<< PLAYER VS PLAYER >>>\nEnter the final extreme" secondExt
    return $ 
        if correctFirstExt <= correctSecondExt then 
            (correctFirstExt, correctSecondExt)
        else 
            (correctSecondExt, correctFirstExt)


getPlayers :: [String] -> IO [String]
getPlayers xs = do
    clear
    putStrLn "<<< PLAYER VS PLAYER >>>\nWirite name of the player and press ENTER. To exit type :exit or :e"
    playerName <- prompt "Player Name: "
    case playerName of
        ":e"    -> return xs
        ":exit" -> return xs 
        _       -> getPlayers (playerName:xs)


winnerGame :: Int -> [String] -> [ValueOrdering] -> IO (Bool, String, [ValueOrdering])
winnerGame _ [] vs = return (False, "", vs)
winnerGame seed (p:ps) vs = do
    clear
    putStrLn $ "<<< PLAYER VS PLAYER >>>\n" ++ "Values entered: " ++ show vs ++ "\nTurn of " ++ p
    input       <- prompt "Enter a value: "
    numberUser  <- controlInput ("<<< PLAYER VS PLAYER >>>\n" ++ "Values entered: " ++ show vs ++ "\nTurn of " ++ p) input
    case compare numberUser seed of
        EQ -> return (True, p, vs)
        LT -> do
            clear
            putStrLn $ "<<< PLAYER VS PLAYER >>>\n" 
                ++ "Values entered: " 
                ++ show vs 
                ++ "\nTurn of " ++ p 
                ++ "\nFail. The value is Bigger"
            _ <- getLine
            winnerGame seed ps ((numberUser, LT):vs)
        GT -> do
            clear
            putStrLn $ "<<< PLAYER VS PLAYER >>>\n" 
                ++ "Values entered: " 
                ++ show vs 
                ++ "\nTurn of " ++ p 
                ++ "\nFail. The value is Smaller"
            _ <- getLine
            winnerGame seed ps ((numberUser, GT):vs)



gamePlayerVsPlayer :: Int -> [String] -> [ValueOrdering] -> IO ()
gamePlayerVsPlayer seed ps vs = do
    (win, p, xs) <- winnerGame seed ps vs
    if win then putStrLn $ "Win player " ++ p else gamePlayerVsPlayer seed ps xs


playerVsPlayer :: IO ()
playerVsPlayer = do
    players         <- getPlayers []
    interval        <- getExtremesOfTheInterval
    randomInterval  <- createSeedValue interval
    gamePlayerVsPlayer (fst randomInterval) players [(fst interval, LT),(snd interval, GT)]



---------------------------------------------------------------------------------------------------
-- MAIN FUNCTION
---------------------------------------------------------------------------------------------------

main :: IO ()
main = do 
    option  <- mainMenu "Choose an option: " optionMainMenu
    if option == "3" then 
        putStrLn "End Game. "
    else do
        switchGame option 
        main
