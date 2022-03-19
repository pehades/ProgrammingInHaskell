import ParserFile

comment :: Parser String
comment = do some (char '-')
             c <- many (sat ( /= '\n'))
             return c