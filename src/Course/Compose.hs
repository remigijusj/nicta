{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Course.Compose where

import Course.Core
import Course.Functor
import Course.Apply
import Course.Applicative
import Course.Bind

-- Exactly one of these exercises will not be possible to achieve. Determine which.

newtype Compose f g a =
  Compose (f (g a))


instance (Functor f, Functor g) => Functor (Compose f g) where
  (<$>) f (Compose a) = Compose ((f <$>) <$> a)

-- t :: a -> b
-- (t <$>) :: g a -> g b
-- ((t <$>) <$>):: f (g a) -> f (g b)


instance (Apply f, Apply g) => Apply (Compose f g) where
  (<*>) (Compose f) (Compose a) = Compose (lift2 (<*>) f a)

-- ttt :: f (g (a -> b))
-- aaa :: f (g a)
-- (<*>) :: g (a -> b) -> g a -> g b 
-- lift2 (<*>) :: f (g (a -> b)) -> f (g a) -> f (g b)


instance (Applicative f, Applicative g) => Applicative (Compose f g) where
  pure = Compose . pure . pure

-- pure :: a -> g a
-- pure . pure :: a -> f (g a)


instance (Bind f, Bind g) => Bind (Compose f g) where
  (=<<) = error "impossible"
