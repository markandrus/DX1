DX1
===

For better-looking documentation, run `haddock -h DX1.hs`.

Format
------

Every line of a `.dx1` file contains a word, its number of occurrences
(in the corpus it originates from), and its pronunciation (as a sequence
of phonemes). Typically, this data is encoded in the form of a
space-separated string. For example:

     A 23310 AH0
     AARON 8 EH1 R AH0 N
     ABANDON 18 AH0 B AE1 N D AH0 N
     ABANDONED 26 AH0 B AE1 N D AH0 N D

In the interest of robustness, this library supports tab-separation
between a word's name, count, and pronunciation, as well as both `DOS`-
and `UNIX`-style newlines.

### data DX1Entry

Stores a word, its number of occurrences, and its phonemes.

#### Instances

* Eq DX1Entry
* Read DX1Entry
* Show DX1Entry

Calculations
------------

### sumCounts :: [DX1Entry] -\> Int

Sum the counts of each `DX1Entry` in a list.

### frequencies :: [DX1Entry] -\> [(DX1Entry, Float)]

Pair each `DX1Entry` in a list with its frequency.

### sortedFrequencies :: [DX1Entry] -\> [(DX1Entry, Float)]

*O(nlog n)*. Sort each `DX1Entry` in a list by its frequency.

Parser
------

### parseDX1 :: String -\> Either ParseError [DX1Entry]

Parses a `.dx1` file to a list of `DX1Entry`s (uses `Parsec`
internally).

Example Program
---------------

### main :: IO ()

*O(nlog n)*. Parses a `.dx1` file from `stdin` or a given filename,
computes the frequency of each word, sorts by frequency in ascending
order, and prints the result.

Produced by [Haddock](http://www.haskell.org/haddock/) version 2.11.0.
Munged by [Pandoc](http://johnmacfarlane.net/pandoc) version 1.9.4.2.
Cleaned by hand.
