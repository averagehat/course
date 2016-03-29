{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}

module Course.Functor where

import Course.Core
import Course.Id
import Course.Optional
import Course.List
import qualified Prelude as P(fmap)

--foldr :: (Foldable t) (a -> b -> b) -> b -> t a -> b
foldr :: (a -> b -> b) -> b -> List a -> b
foldr _ z Nil = z
foldr f z (x:.xs) = (f x $ foldr f z xs)

-- | All instances of the `Functor` type-class must satisfy two laws. These laws
-- are not checked by the compiler. These laws are given as:
--
-- * The law of identity
--   `∀x. (id <$> x) ≅ x`
--
-- * The law of composition
--   `∀f g x.(f . g <$> x) ≅ (f <$> (g <$> x))`
class Functor f where
  -- Pronounced, eff-map.
  (<$>) ::
    (a -> b)
    -> f a
    -> f b

infixl 4 <$>

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Course.Core
-- >>> import qualified Prelude as P(return, (>>))

-- | Maps a function on the Id functor.
--
-- >>> (+1) <$> Id 2
-- Id 3
--
fmapId :: (a -> b) -> Id a -> Id b
fmapId f (Id x) = Id (f x)

instance Functor Id where
  (<$>) ::
    (a -> b)
    -> Id a
    -> Id b
  (<$>) = fmapId
    

fmapList :: (a -> b) -> List a -> List b
fmapList f xs = foldr go Nil xs
  where go x z = (f x) :. z

-- | Maps a function on the List functor.
--
-- >>> (+1) <$> Nil
-- []
--
-- >>> (+1) <$> (1 :. 2 :. 3 :. Nil)
-- [2,3,4]
instance Functor List where
  (<$>) ::
    (a -> b)
    -> List a
    -> List b
  (<$>) = fmapList

-- | Maps a function on the Optional functor.
--
-- >>> (+1) <$> Empty
-- Empty
--
-- >>> (+1) <$> Full 2
-- Full 3
--
fmapOptional :: (a -> b) -> Optional a -> Optional b
fmapOptional _ Empty = Empty
fmapOptional f (Full x) = Full (f x)

instance Functor Optional where
  (<$>) ::
    (a -> b)
    -> Optional a
    -> Optional b
  (<$>) = fmapOptional

fmapReader :: (a -> b) -> ((->) t a) -> ((->) t b)
fmapReader f a = (\x -> f $ a x)
-- | Maps a function on the reader ((->) t) functor.
--
-- >>> ((+1) <$> (*2)) 8
-- 17
instance Functor ((->) t) where
  (<$>) ::
    (a -> b)
    -> ((->) t a)
    -> ((->) t b)
  (<$>) = fmapReader

afmap :: Functor f => a -> f b -> f a
afmap x fb =(const x) <$>  fb -- const :: a -> b -> a
--afmap a fb = fb <$> (\_ -> a) -- const :: a -> b -> a
-- | Anonymous map. Maps a constant value on a functor.
--
-- >>> 7 <$ (1 :. 2 :. 3 :. Nil)
-- [7,7,7]
--
-- prop> x <$ (a :. b :. c :. Nil) == (x :. x :. x :. Nil)
--
-- prop> x <$ Full q == Full x
(<$) ::
  Functor f =>
  a
  -> f b
  -> f a
(<$) = afmap

infixl 4 <$
-- | Anonymous map producing unit value.
--
-- >>> void (1 :. 2 :. 3 :. Nil)
-- [(),(),()]
--
-- >>> void (Full 7)
-- Full ()
--
-- >>> void Empty
-- Empty
--
-- >>> void (+10) 5
-- ()
void ::
  Functor f =>
  f a
  -> f ()
void = (() <$)
  

-----------------------
-- SUPPORT LIBRARIES --
-----------------------

-- | Maps a function on an IO program.
--
-- >>> reverse <$> (putStr "hi" P.>> P.return ("abc" :: List Char))
-- hi"cba"
instance Functor IO where
  (<$>) =
    P.fmap
