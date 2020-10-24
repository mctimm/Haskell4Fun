import Data.Char
import System.Random
import System.IO

showlogo = "--------------------------------------------\n| #  #   #   #   #  #### #   #   #   #   # |\n| #  #  # #  ##  # #     ## ##  # #  ##  # |\n| #### ##### # # # #  ## # # # ##### # # # |\n| #  # #   # #  ## #   # #   # #   # #  ## |\n| #  # #   # #   #  ###  #   # #   # #   # |\n--------------------------------------------\n\n"

splitString elem string =
    let helper [] elem [] = []
        helper current elem [] = (current):[]
        helper current elem (x:xs) = if x == elem
            then current:(helper [] elem xs)
            else helper (current ++ (x:[])) elem xs
    in helper [] elem string

printGame 0 = "Amount of wrong letters: 0\n\n\n\n\n\n\n\n____________\n\n"
printGame 1 = "Amount of wrong letters: 1\n\n\n  |\n  |\n  |\n  |\n  |\n__|_________\n\n"
printGame 2 = "Amount of wrong letters: 2\n\n  _______\n  |\n  |\n  |\n  |\n  |\n__|_________\n\n"
printGame 3 = "Amount of wrong letters: 3\n\n  _______\n  |/\n  |\n  |\n  |\n  |\n__|_________\n\n"
printGame 4 = "Amount of wrong letters: 4\n\n  _______\n  |/   | \n  |    O \n  |\n  |\n  |\n__|_________\n\n"
printGame 5 = "Amount of wrong letters: 5\n\n  _______\n  |/   | \n  |    O \n  |    |\n  |    |\n  |\n__|_________\n\n"
printGame 6 = "Amount of wrong letters: 6\n\n  _______\n  |/   | \n  |    O \n  |   \\|\n  |    | \n  |\n__|_________\n\n"
printGame 7 = "Amount of wrong letters: 7\n\n  _______\n  |/   | \n  |    O \n  |   \\|/\n  |    | \n  |\n__|_________\n\n"
printGame 8 = "Amount of wrong letters: 8\n\n  _______\n  |/   | \n  |    O \n  |   \\|/\n  |    | \n  |   /\n__|_________\n\n"
printGame 9 = "Amount of wrong letters: 9\n\n  _______\n  |/   | \n  |    O \n  |   \\|/\n  |    | \n  |   / \\\n__|_________\n\n"
printGame 10 = "Amount of wrong letters: 10\n\n  _______\n  |/   | \n  |    X \n  |   \\|/\n  |    | \n  |   / \\\n__|_________\n\n"
winner = "---------------\n--- Results ---\n---------------\n\nCongratulations you guessed the right word!\n\n"
loser word = "---------------\n--- Results ---\n---------------\n\nYou guessed the wrong word. The word was " ++ word ++ ". better luck next time!\n\n"
notALetter = "Only alphanumeric symbols are allowed (a-z, A-Z), try again:\n"

play errors answer current [] turns skiptext =
    if (answer == current)
        then putStr winner
        else if (errors >= 10)
            then putStr $ loser answer
            else do
                if skiptext
                    then putStr ""
                    else putStr $ (show turns) ++ ".     Enter the letter(s) you want to guess: "
                hFlush stdout
                input <- getLine
                play errors answer current input turns (null input)
play errors answer current (x:xs) turns skiptext =
    if (answer == current)
        then putStr winner
        else if (errors >= 10)
            then putStr $ loser answer
            else if (isLetter x)
                    then if (toLower x) `elem` answer
                        then do
                            let newC = insertLetters (toLower x) answer current
                            putStrLn "\nThat letter was correct.\n"
                            putStrLn $ "The word including the letters you guessed: " ++ newC ++ "\n"
                            putStr $ printGame errors
                            if not (((null xs) || (errors >= 10) || skiptext) || current == answer) 
                                then putStr $ (show (turns + 1)) ++ ".     Enter the letter(s) you want to guess: "
                                else putStr ""
                            play errors answer newC xs (turns + 1) False
                        else do
                        putStrLn "\nThat letter was incorrect.\n"
                        putStrLn $ "The word including the letters you guessed: " ++ current ++ "\n"
                        putStr $ printGame (errors + 1)
                        if not (((null xs) || ((errors + 1) >= 10) || skiptext) || current == answer)
                            then putStr $ (show (turns + 1)) ++ ".     Enter the letter(s) you want to guess: "
                            else putStr ""
                        play (errors + 1) answer current xs (turns + 1) False
                    else do
                        putStr notALetter
                        play errors answer current xs turns True

insertLetters x [] [] = []
insertLetters x (y:ys) (z:zs) = if x == y
    then y:(insertLetters x ys zs)
    else z:(insertLetters x ys zs)

main = do
    handle <- openFile "words.txt" ReadMode
    wordsString <- hGetLine handle
    let wordsList = splitString '|' wordsString
    index <- randomRIO (0,(length wordsList) -1)
    let word = wordsList !! index
        current = take (length word) $ repeat '.'
    hClose handle
    putStrLn $ "guessWord = " ++ word
    putStr showlogo
    putStrLn "Welcome to the game Hangman!\n"
    putStrLn "The objective in this game is to guess the word."
    putStrLn "You can enter both uppercase and lowercase letters."
    putStrLn "If you think you know the word, you can type it in."
    putStrLn "You will lose if you have guessed 10 letters wrong.\n"
    putStrLn $ "This is the word you need to guess: " ++ current ++ "\n"
    play 0 word current [] 1 False
    