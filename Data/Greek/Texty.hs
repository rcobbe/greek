{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

-- | Define a general interface to a type that is similar to 'Data.Text.Text',
--   to allow us to defer the decision about the precise string representation
--   as late as possible.  There is an unfortunate interaction with overloaded
--   strings, which requires clients to add type annotations in places where
--   they didn't previously need to, but this is a relatively minor cost.

module Data.Greek.Texty(Texty(..)) where

import qualified Data.Text as Text
import qualified Data.Text.Lazy as Lazy

-- | General interface to "texty" values -- that is, a value that contains some
--   text and behaves generally like a string.  The set of operations supported
--   is ad-hoc, consisting only of the operations we actually need in this
--   package.
class Texty a where
  -- | Convert a string to the texty type
  pack :: String -> a
  -- | Convert a texty type to the equivalent string
  unpack :: a -> String

  -- | The type-appropriate representation of the empty string
  empty :: a

  -- | Add a character to the front of a texty value
  cons :: Char -> a -> a

  -- | Tests whether a texty value is empty or not
  null :: a -> Bool

  -- | Returns the first character of a texty value, which must not be empty.
  head :: a -> Char

  -- | Returns all characters after the head of a texty value, which must not
  --   be empty.
  tail :: a -> a

  -- | Concatenates two texty values.
  append :: a -> a -> a

  -- | @span p t@ returns a pair of texty values.  The first is the longest
  --   prefix of @t@ that contains only characters that satisfy @p@, and the
  --   second is the remainder of the original texty value.
  span :: (Char -> Bool) -> a -> (a, a)

instance Texty String where
  pack = id
  unpack = id

  empty = []
  cons = (:)
  null = Prelude.null
  head = Prelude.head
  tail = Prelude.tail

  append = (++)

  span = Prelude.span

instance Texty Text.Text where
  pack = Text.pack
  unpack = Text.unpack

  empty = Text.empty
  cons = Text.cons
  null = Text.null
  head = Text.head
  tail = Text.tail

  append = Text.append

  span = Text.span

instance Texty Lazy.Text where
  pack = Lazy.pack
  unpack = Lazy.unpack

  empty = Lazy.empty
  cons = Lazy.cons
  null = Lazy.null
  head = Lazy.head
  tail = Lazy.tail

  append = Lazy.append

  span = Lazy.span
