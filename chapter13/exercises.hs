import ParserFile
import Control.Applicative

-- exercise 1

comment :: Parser String
comment = do some (char '-')
             c <- many (sat ( /= '\n'))
             return c

-- exercise 6
-- a minor deviation is that subtraction associates to the left a - b - c = (a - b) - c.
-- so a better implementation should take care of that

eval :: String -> Int
eval xs = case (parse expr xs) of
            [(n, [])] -> n
            [(_, out)] -> error ("Unused input" ++ out)
            [] -> error "Invalid Input"
--
expr :: Parser Int
expr = do t <- term
          do symbol "+"
             e <- expr
             return (t + e)
           <|> do symbol "-"
                  e <- expr
                  return (t - e)
                <|> return t

term :: Parser Int
term = do f <- factor
          do symbol "*"
             t <- term
             return (f * t)
           <|> do symbol "/"
                  t <- term
                  return (f `div` t)
                <|> return f

factor :: Parser Int
factor = do symbol "("
            e <- expr
            symbol ")"
            return e
          <|> natural