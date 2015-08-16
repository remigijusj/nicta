{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}

module Course.Extend where

import Course.Core
import Course.Id
import Course.List
import Course.Optional
import Course.Functor

-- | All instances of the `Extend` type-class must satisfy one law. This law
-- is not checked by the compiler. This law is given as:
--
-- * The law of associativity
--   `∀f g x. (f <<=) . (g <<=) ≅ (<<=) (f . (g <<=) <<=)

class Functor f => Extend f where
  (<<=) :: (f a -> b) -> f a -> f b -- Pronounced, extend.

infixr 1 <<=


-- | Implement the @Extend@ instance for @Id@.
--
-- >>> id <<= Id 7
-- Id (Id 7)

instance Extend Id where
  (<<=) :: (Id a -> b) -> Id a -> Id b
  (<<=) f aa = Id (f aa)


-- | Implement the @Extend@ instance for @List@.
--
-- >>> length <<= ('a' :. 'b' :. 'c' :. Nil)
-- [3,2,1]
--
-- >>> id <<= (1 :. 2 :. 3 :. 4 :. Nil)
-- [[1,2,3,4],[2,3,4],[3,4],[4]]
--
-- >>> reverse <<= ((1 :. 2 :. 3 :. Nil) :. (4 :. 5 :. 6 :. Nil) :. Nil)
-- [[[4,5,6],[1,2,3]],[[4,5,6]]]

instance Extend List where
  (<<=) :: (List a -> b) -> List a -> List b
  (<<=) f Nil = Nil
  (<<=) f as@(_ :. t) = f as :. (f <<= t)


-- | Implement the @Extend@ instance for @Optional@.
--
-- >>> id <<= (Full 7)
-- Full (Full 7)
--
-- >>> id <<= Empty
-- Empty

instance Extend Optional where
  (<<=) :: (Optional a -> b) -> Optional a -> Optional b
  (<<=) f Empty = Empty
  (<<=) f o@(Full _) = Full (f o)


-- | Duplicate the functor using extension.
--
-- >>> cojoin (Id 7)
-- Id (Id 7)
--
-- >>> cojoin (1 :. 2 :. 3 :. 4 :. Nil)
-- [[1,2,3,4],[2,3,4],[3,4],[4]]
--
-- >>> cojoin (Full 7)
-- Full (Full 7)
--
-- >>> cojoin Empty
-- Empty

cojoin :: Extend f => f a -> f (f a)
cojoin = (id <<=)
