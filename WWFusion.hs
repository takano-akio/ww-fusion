{-# LANGUAGE RankNTypes, ExistentialQuantification #-}
module WWFusion
  ( foldrW
  , buildW
  , foldl
  , foldl'
  , foldr
  , filter
  , map
  , eft
  , (++)
  , concat
  , Wrap(..)
  ) where

import Prelude hiding ((++), foldl, foldr, concat, filter, map)

data Wrap f b = Wrap (forall e. f e -> (e -> b -> b)) (forall e. (e -> b -> b) -> f e)

foldrW
  :: Wrap f b
  -> (a -> b -> b)
  -> b
  -> [a]
  -> b
foldrW (Wrap wrap unwrap) f z0 list0 = wrap go list0 z0
  where
    go = unwrap $ \list z' -> case list of
      [] -> z'
      x:xs -> f x $ wrap go xs z'
{-# NOINLINE[0] foldrW #-}

newtype Simple b e = Simple { runSimple :: e -> b -> b }

isoSimple :: Wrap (Simple b) b
isoSimple = Wrap runSimple Simple

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f z = foldrW isoSimple f z
{-# INLINE foldr #-}

buildW
  :: (forall b f . (Wrap f b)
    -> (a -> b -> b)
    -> b
    -> b)
  -> [a]
buildW g = g isoSimple (:) []
{-# INLINE[0] buildW #-}

augmentW
  :: (forall b f . (Wrap f b)
    -> (a -> b -> b)
    -> b
    -> b)
  -> [a]
  -> [a]
augmentW g xs = g isoSimple (:) xs
{-# INLINE[0] augmentW #-}

(++) :: [a] -> [a] -> [a]
a ++ b = augmentW (\i c n -> foldrW i c n a) b
{-# INLINE (++) #-}

concat :: [[a]] -> [a]
concat xs = buildW (\i c n -> foldrW i (\x y -> foldrW i c y x) n xs)
{-# INLINE concat #-}

foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' f initial = \xs -> foldrW (Wrap wrap unwrap) g id xs initial
  where
    wrap (Simple s) e k a = k $ s e a
    unwrap u = Simple $ \e a -> u e id a
    g x next acc = next $! f acc x
{-# INLINE foldl' #-}

foldl :: (b -> a -> b) -> b -> [a] -> b
foldl f initial = \xs -> foldrW (Wrap wrap unwrap) g id xs initial
  where
    wrap (Simple s) e k a = k $ s e a
    unwrap u = Simple $ \e a -> u e id a
    g x next acc = next $ f acc x
{-# INLINE foldl #-}

map :: (a -> b) -> [a] -> [b]
map f = \xs -> buildW (mapFB f xs)

mapFB
  :: (a -> b)
  -> [a]
  -> Wrap f r
  -> (b -> r -> r)
  -> r
  -> r
mapFB f xs ww cons nil = foldrW ww (cons . f) nil xs

filter :: (a -> Bool) -> [a] -> [a]
filter p = \xs -> buildW (\ww cons nil -> foldrW ww (f cons) nil xs)
  where
    f cons x y = if p x then cons x y else y
{-# INLINE filter #-}

eft :: Int -> Int -> [Int]
eft = \from to -> buildW (eftFB from to)
{-# INLINE eft #-}

eftFB
  :: Int
  -> Int
  -> (Wrap f r)
  -> (Int -> r -> r)
  -> r
  -> r
eftFB from to (Wrap wrap unwrap) cons nil = wrap go from nil
  where
    go = unwrap $ \i rest -> if i <= to
      then cons i $ wrap go (i + 1) rest
      else rest
{-# INLINE[0] eftFB #-}

{-# RULES
"foldrW/buildW" forall
    f z
    (i :: Wrap f b)
    (g :: forall c g .
      (Wrap g c)
      -> (a -> c -> c)
      -> c
      -> c)
    .
  foldrW i f z (buildW g) = g i f z
"foldrW/augmentW" forall
    f z
    (i :: forall e. Wrap (f e) (e -> b -> b))
    (g :: forall c g .
      (Wrap g c)
      -> (a -> c -> c)
      -> c
      -> c)
    xs
    .
  foldrW i f z (augmentW g xs) = g i f (foldrW i f z xs)
"augmentW/buildW" forall
    (f :: forall c g.
      (Wrap g c)
      -> (a -> c -> c)
      -> c
      -> c)
    (g :: forall c g .
      (Wrap g c)
      -> (a -> c -> c)
      -> c
      -> c)
    .
  augmentW g (buildW f) = buildW (\i c n -> g i c (f i c n))
  #-}
