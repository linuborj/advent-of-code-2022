module Text.ParserCombinators.ReadP.Unambiguous
  ( ReadP
  , nat
  , int
  , many
  , many1
  , sepBy
  , sepBy1
  , unambiguosParser
  , string
  , char
  , readP_to_S
  , readS_to_P
  , (<++)
  , munch
  , munch1
  , satisfy
  ) where

import Text.ParserCombinators.ReadP hiding (many, many1, sepBy, sepBy1)
import qualified Text.ParserCombinators.ReadP as ReadP
import qualified Data.Char as Char

nat :: (Integral a, Read a) => ReadP a
nat = read <$> many1 (satisfy Char.isDigit)

int :: (Integral a, Read a) => ReadP a
int = foldl1 (<++)
  [ nat
  , negate <$> (char '-' *> nat)
  ]

many :: ReadP a -> ReadP [a]
many parser = foldl1 (<++)
  [ (:) <$> parser <*> many parser
  , return []
  ]

many1 :: ReadP a -> ReadP [a]
many1 parser = (:) <$> parser <*> many parser

sepBy :: ReadP a -> ReadP b -> ReadP [a]
sepBy parser separator = foldl1 (<++)
  [ (:) <$> parser <*> many (separator *> parser)
  , return []
  ]

sepBy1 :: ReadP a -> ReadP b -> ReadP [a]
sepBy1 parser separator = (:) <$> parser <*> many (separator *> parser)

unambiguosParser :: ReadP a -> String -> Maybe a
unambiguosParser parser input = case ReadP.readP_to_S parser input of
  [(parsed, "")] -> Just parsed
  _              -> Nothing

