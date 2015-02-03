module Utils.File where

{- Modules -}

import Data.List
import Text.ParserCombinators.Parsec

import qualified Data.List.Split as S

{- Utilities -}

-- CSV File Parser (Real World Haskell) --

csvFile = endBy line eol
line    = sepBy cell (char ',')
cell    = quotedCell <|> many (noneOf ",\n\r")

quotedCell = 
    do char '"'
       content <- many quotedChar
       char '"' <?> "quote at end of cell"
       return content

quotedChar =
        noneOf "\""
    <|> try (string "\"\"" >> return '"')

eol =   try (string "\n\r")
    <|> try (string "\r\n")
    <|> string "\n"
    <|> string "\r"
    <?> "end of line"

{- Functions -}

-- File Parsing --

parseCSV :: String -> Either ParseError [[String]]
parseCSV input = parse csvFile "(unknown)" input

parseFile :: FilePath -> IO [[String]]
parseFile rp = 
    do c <- readFile rp
       case parseCSV c of
            Right r -> return r

-- File IO --

makeWritePaths :: [FilePath] -> [(FilePath, String)] -> IO [(FilePath, String)]
makeWritePaths (r:rs) result = do
    let part = S.splitOn "/" r
    let outl = take (length part) part
    let name = init $ S.splitOn "." $ last $ outl
    let fold = (init $ init $ outl) ++ ["results"]
    let file = intercalate "." $ name ++ ["txt"]
    let dirs = (intercalate "/" fold) ++ "/"
    let path = dirs ++ file
    let retn = (path, concat name)
    let resa = (retn:result)
    makeWritePaths rs resa
makeWritePaths [] result = return $ reverse result
