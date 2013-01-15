{-#LANGUAGE NoImplicitPrelude #-}

module Main
  ( DX1Entry
  , parseDX1File
  , main
  ) where

import Control.Applicative
import Data.List hiding (words)
import Text.ParserCombinators.Parsec hiding ((<|>), many)
import Prelude hiding (words)

-- | Contains a word, its number of occurences, and phonemes.
data DX1Entry = DX1Entry
  { _word     :: String
  , _count    :: Int
  , _phonemes :: [String] }
  deriving (Eq)

-- | Show a 'DX1Entry' in @.dx1@ format.
instance Show DX1Entry where
  show (DX1Entry word count phonemes) =
    word       ++ " " ++
    show count ++ " " ++
    (concat $ intersperse " " phonemes)

-- | Matches a 'DX1Entry'.
dx1Entry = pure DX1Entry
  <*> word   <* space
  <*> digits <* space
  <*> words  <* eol
                        
-- | Matches many 'DX1Entry's.
dx1File  = many dx1Entry

-- | Parses a @.dx1@ file to a list of 'DX1Entry's.
parseDX1File :: String -> Either ParseError [DX1Entry]
parseDX1File = parse dx1File "(unknown)"

-- | Matches a word.
word = many1 $ noneOf " \n\r"

-- | Matches words.
words = many $ word <* space

-- | Matches an integer.
digits = pure read <*> many1 digit

-- | Matches both @DOS@- and @UNIX@-style newlines.
eol = try (string "\r\n")
   <|> string "\n"
   <|> string "\r"
   <?> "end of line"

-- | Parse a @.dx1@ file from @stdin@ or a given filename.
main :: IO ()
main = do
  file <- getContents
  case parseDX1File file of
    Left  e -> putStrLn "Error parsing input:" >> print e
    Right r -> mapM_ print r
