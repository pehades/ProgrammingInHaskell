import Prelude hiding (getLine)
import Data.Char

--exercise 1
putStrRedefined :: String -> IO ()
putStrRedefined xs = sequence_ [putChar x | x <- xs]

--exercise 2
type Board = [Int]

putRow :: Int -> Int -> IO ()
putRow r n = do putStr (show r)
                putStr ": "
                putStrLn (concat (replicate n "* "))

putBoard :: Board -> IO ()
putBoard [] = putStr ""
putBoard (x:xs) = do putRow (1 + length xs) x
                     putBoard xs

-- exercise 3
putBoardWithSequence :: Board -> IO ()
putBoardWithSequence board = sequence_ [putRow r n | (r, n) <- zip [1..] board]

--exercise 4

getSequentialIntsFromConsole :: Int -> IO [Int]
getSequentialIntsFromConsole 0 = do putChar '\n'
                                    return []
getSequentialIntsFromConsole n = do x <- getChar
                                    if isDigit x then
                                      do xs <- getSequentialIntsFromConsole (n-1)
                                         return ((digitToInt x): xs)
                                    else
                                      getSequentialIntsFromConsole n

adder :: IO ()
adder = do putStr "How many numbers? "
           num <- getChar
           if isDigit num then
             do xs <- getSequentialIntsFromConsole (digitToInt num)
                putStr "The total is "
                putStrLn (show (sum (xs)))
           else
             putStr "error"


