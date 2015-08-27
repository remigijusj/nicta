{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Course.FastAnagrams where

import Course.Core
import Course.List
import Course.Functor
import qualified Data.Set as S


newtype NoCaseString =
  NoCaseString {
    ncString :: Chars
  }

instance Eq NoCaseString where
  (==) = (==) `on` map toLower . ncString

instance Show NoCaseString where
  show = show . ncString


-- Return all anagrams of the given string
-- that appear in the given dictionary file.

fastAnagrams :: Chars -> Filename -> IO (List Chars)
fastAnagrams word fn =
  let perms = permutations word
      analyse = flip (filter . flip S.member) perms . S.fromList . hlist
   in analyse . lines <$> (readFile fn)

-- not doen: copied analyse code
