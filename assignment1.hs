module Main where

{- This is a framework in which all functions to be written are "undefined".  -
 - Note that in most cases parameters, pattern-matching and guards have been  -
 - omitted! You will have to add those yourself.                              -}

import Data.Char
import Data.List
import Data.Maybe

-- | Model

type Field = String
type Row   = [Field]
type Table = [Row]

-- | Main

main :: IO ()
main = interact (unlines . exercise . lines)

exercise :: [String] -> [String]
exercise = printTable
         . project ["last", "first", "salary"]
         . select "gender" "male"
         . parseTable

-- | Parsing

-- * Exercise 1

parseTable :: [String] -> Table
parseTable strings = (map strToRow strings)
-- parseTable (x:xs) = words x + 

strToRow :: String -> Row
strToRow input = words input

-- | Printing

-- * Exercise 2

printLine :: [Int] -> String
printLine l = "+" ++ intercalate "+" (map (flip replicate '-') l) ++ "+"


-- * Exercise 3
printField :: Int -> String -> String
printField l s@(x:xs) | l == (length s) = s
                      | isDigit (last s) = printField l (" " ++ s)
                      | otherwise = printField l (s ++ " ")

-- * Exercise 4

printRowHelper :: (Int, String) -> String
printRowHelper (i, str) = printField i str

printRow :: [(Int, String)] -> String
printRow l = "+" ++ intercalate "|" (map printRowHelper l) ++ "+"

-- * Exercise 5

columnWidths :: Table -> [Int]
columnWidths l = map maximum (transpose ((map . map) length l))

-- * Exercise 6

printTable :: Table -> [String]
printTable table@(header:rows)
    = map printRow (map (zip widths) table) ++ (printLine table)
    where widths = columnWidths table

-- | Querying

-- * Exercise 7

select :: Field -> Field -> Table -> Table
select column value table@(header:rows)
    = undefined

-- * Exercise 8

project :: [Field] -> Table -> Table
project columns table@(header:_)
    = undefined
