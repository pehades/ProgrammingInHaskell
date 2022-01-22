import Data.Char

next :: Int -> Int
next 1 = 2
next 2 = 1

type Board = [Int]

initial :: Board
initial = [5, 4, 3, 2, 1]

finished :: Board -> Bool
finished board = all (==0) board

valid :: Board -> Int -> Int -> Bool
valid board row num = board !! (row - 1) >= num

-- notice that move does allow removing more elements than they exist on a specific row
move :: Board -> Int -> Int -> Board
move board row num = [update r n | (r, n) <- zip [1..] board]
                   where update r n = if r == row then n - num else n

putRow :: Int -> Int -> IO ()
putRow r n = do putStr (show r)
                putStr ": "
                putStrLn (concat (replicate n "* "))

putBoard :: Board -> IO ()
putBoard [] = putStr ""
putBoard (x:xs) = do putRow (1 + length xs) x
                     putBoard xs

newline :: IO ()
newline = putChar '\n'

getDigit :: String -> IO Int
getDigit prompt = do putStr prompt
                     x <- getChar
                     newline
                     if isDigit x then
                       return (digitToInt x)
                     else
                       do putStrLn "Invalid digit"
                          getDigit prompt

play :: Board -> Int -> IO ()
play board player = do newline
                       putBoard board
                       if finished board then
                         do
                           putStr "Player "
                           putStr (show (next player))
                           putStrLn " wins!"
                       else
                         do row <- getDigit "Pick a row: "
                            num <- getDigit "Number of * to remove: "
                            if valid board row num then
                              play (move board row num) (next player)
                            else
                              do newline
                                 putStrLn "Invalid move"
                                 play board player

