{-#LANGUAGE NoImplicitPrelude, TemplateHaskell #-}

module DX1
  ( -- * Format
    -- $format
    DX1Entry
    -- ** Calculations
  , sumCounts
  , frequencies
  , sortedFrequencies
    -- * Parser
  , parseDX1
    -- * Example Program
  , main
  ) where

import Control.Applicative
import Control.Lens
import Control.Monad ((>=>), forM, liftM, join)
import Data.Function (on)
import Data.List (intersperse, sortBy)
import System.Environment
import Text.ParserCombinators.Parsec hiding ((<|>), many, count, optional)
import Text.Parsec.Prim (ParsecT)
import Prelude hiding (words)

{- Format -}

-- $format Every line of a @.dx1@ file contains a word, its number of
-- occurrences (in the corpus it originates from), and its pronunciation (as a
-- sequence of phonemes). Typically, this data is encoded in the form of a
-- space-separated string. For example:
--
-- > A 23310 AH0
-- > AARON 8 EH1 R AH0 N
-- > ABANDON 18 AH0 B AE1 N D AH0 N
-- > ABANDONED 26 AH0 B AE1 N D AH0 N D
--
-- In the interest of robustness, this library supports tab-separation between
-- a word's name, count, and pronunciation, as well as both @DOS@- and
-- @UNIX@-style newlines.


{- Datatype -}

-- | Stores a word, its number of occurrences, and its phonemes.
data DX1Entry = DX1Entry
  { _name     :: String
  , _count    :: Int
  , _phonemes :: [String] }
  deriving (Eq)

makeLenses ''DX1Entry


{- Instances -}

-- NOTE: This uses the 'ParsecRead' trick specialized to 'DX1Entry' so as not to
-- require FlexibleInstances and UndecidableInstances.

-- | Read a @.dx1@ format 'DX1Entry' (uses 'Parsec' internally).
instance Read DX1Entry where
  readsPrec _ = either (const []) id . parse parsecRead "" where
    parsecRead = do a <- dx1Entry; rest <- getInput; return [(a, rest)]

-- | Show a 'DX1Entry' in @.dx1@ format.
instance Show DX1Entry where
  show (DX1Entry name count phonemes) =
    name       ++ " " ++
    show count ++ " " ++
    unwords phonemes


{- Calculations -}

-- | Sum the counts of each 'DX1Entry' in a list.
sumCounts :: [DX1Entry] -> Int
sumCounts = sumOf (folded . count)

-- | Pair each 'DX1Entry' in a list with its frequency.
frequencies :: [DX1Entry] -> [(DX1Entry, Float)]
frequencies entries =
    let n = fromIntegral $ sumCounts entries
    in  map (\e -> (e, frequency n e)) entries
  where
    frequency n e = fromIntegral (e ^. count) / n

-- | /O(nlog n)/. Sort each 'DX1Entry' in a list by its frequency.
sortedFrequencies :: [DX1Entry] -> [(DX1Entry, Float)]
sortedFrequencies = sortBy (compare `on` snd) . frequencies


{- Parsers -}

-- | Matches a 'DX1Entry'.
dx1Entry = pure DX1Entry
  <*> word   <* sep
  <*> digits <* sep
  <*> words  <* eol
                        
-- | Matches many 'DX1Entry's.
dx1File = dx1Entry `manyTill` eof

-- | Parses a @.dx1@ file to a list of 'DX1Entry's (uses 'Parsec' internally).
parseDX1 :: String -> Either ParseError [DX1Entry]
parseDX1 = parse dx1File ""

-- | Matches a word.
word = many1 $ noneOf " \n\r"

-- | Matches a space-separated list of words.
words = word `sepBy1` char ' '

-- | Matches an integer.
digits = pure read <*> many1 digit

-- | Matches either a space or a tab.
sep = oneOf " \t"

-- | Matches both @DOS@- and @UNIX@-style newlines.
eol = try (string "\r\n")
   <|> string "\n"
   <|> string "\r"


{- Example Program -}

-- | /O(nlog n)/. Parses a @.dx1@ file from @stdin@ or a given filename,
-- computes the frequency of each word, sorts by frequency in ascending order,
-- and prints the result.
main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> (liftM parseDX1Freqs >=> mapM_ putStrLn) getContents
    paths -> mapM_ ((liftM parseDX1Freqs >=> mapM_ putStrLn) . readFile) paths

parseDX1Freqs file = case parseDX1 file of
  Left  e -> return $ "Error parsing input: " ++ show e
  Right r -> join
           . forM (sortedFrequencies r) $ \(d, f) ->
               return $ (d ^. name) ++ " " ++ show f
