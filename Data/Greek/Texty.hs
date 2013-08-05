{-# LANGUAGE OverloadedStrings #-}

-- This was an attempt to use type classes to defer the decision of lazy vs
-- strict Text as late as possible.  Unfortunately, it doesn't work well with
-- overloaded strings: in @f "hello, world"@ the compiler looks for a Texty
-- instance for [Char], which isn't what we want.

module Data.Greek.Texty(Texty) where

import Control.DeepSeq (NFData)
import Data.Data (Data)
import Data.Int (Int64)
import Data.Monoid (Monoid)
import Data.String (IsString)
import Data.Typeable (Typeable)

import qualified Data.Text as Text
import qualified Data.Text.Lazy as Lazy

class (Eq a, Data a, Ord a, Read a, Show a,
       Typeable a, IsString a, NFData a, Monoid a) => Texty a where

  pack :: String -> a
  unpack :: a -> String
  singleton :: Char -> a
  empty :: a

  cons :: Char -> a -> a
  snoc :: a -> Char -> a
  append :: a -> a -> a
  uncons :: a -> Maybe (Char, a)
  head :: a -> Char
  last :: a -> Char
  tail :: a -> a
  init :: a -> a
  null :: a -> Bool
  length :: a -> Int64
  compareLength :: a -> Int64 -> Ordering

instance Texty Text.Text where
  pack = Text.pack
  unpack = Text.unpack
  singleton = Text.singleton
  empty = Text.empty

  cons = Text.cons
  snoc = Text.snoc
  append = Text.append
  uncons = Text.uncons
  head = Text.head
  last = Text.last
  tail = Text.tail
  init = Text.init
  null = Text.null
  length t = fromInteger (toInteger (Text.length t))
  compareLength t n =
    Text.compareLength t (fromInteger (toInteger n))

instance Texty Lazy.Text where
  pack = Lazy.pack
  unpack = Lazy.unpack
  singleton = Lazy.singleton
  empty = Lazy.empty

  cons = Lazy.cons
  snoc = Lazy.snoc
  append = Lazy.append
  uncons = Lazy.uncons
  head = Lazy.head
  last = Lazy.last
  tail = Lazy.tail
  init = Lazy.init
  null = Lazy.null
  length = Lazy.length
  compareLength = Lazy.compareLength

f :: Texty t => t -> String
f = unpack
