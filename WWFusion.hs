{-# LANGUAGE RankNTypes #-}
module WWFusion
  ( foldrW
  , buildW
  , foldl'
  , foldr
  , (++)
  , concat
  , Iso(..)
  ) where

import Prelude hiding ((++), foldr, concat)

data Iso a b = Iso (a -> b) (b -> a)

foldrW
  :: (forall e. Iso (f e) (e -> b -> b))
  -> (a -> b -> b)
  -> b
  -> [a]
  -> b
foldrW (Iso wrap unwrap) f z0 list0 = wrap go list0 z0
  where
    go = unwrap $ \list z' -> case list of
      [] -> z'
      x:xs -> f x $ wrap go xs z'
{-# NOINLINE[0] foldrW #-}

newtype Simple b e = Simple { runSimple :: e -> b -> b }

isoSimple :: Iso (Simple b e) (e -> b -> b)
isoSimple = Iso runSimple Simple

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f z = foldrW isoSimple f z
{-# INLINE foldr #-}

buildW
  :: (forall b f
    .  (forall e. Iso (f e) (e -> b -> b))
    -> (a -> b -> b)
    -> b
    -> b)
  -> [a]
buildW g = g isoSimple (:) []
{-# INLINE[0] buildW #-}

augmentW
  :: (forall b f
    .  (forall e. Iso (f e) (e -> b -> b))
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
foldl' f initial xs = foldrW (Iso wrap unwrap) g id xs initial
  where
    wrap (Simple s) e k a = k $ s e a
    unwrap u = Simple $ \e -> u e id
    g x next acc = next $! f acc x
{-# INLINE foldl' #-}

{-# RULES
"foldrW/buildW" forall
    f z
    (i :: forall e. Iso (f e) (e -> b -> b))
    (g :: forall c g
      .  (forall e. Iso (g e) (e -> c -> c))
      -> (a -> c -> c)
      -> c
      -> c)
    .
  foldrW i f z (buildW g) = g i f z
"foldrW/augmentW" forall
    f z
    (i :: forall e. Iso (f e) (e -> b -> b))
    (g :: forall c g
      .  (forall e. Iso (g e) (e -> c -> c))
      -> (a -> c -> c)
      -> c
      -> c)
    xs
    .
  foldrW i f z (augmentW g xs) = g i f (foldrW i f z xs)
"augmentW/buildW" forall
    (f :: forall c g
      .  (forall e. Iso (g e) (e -> c -> c))
      -> (a -> c -> c)
      -> c
      -> c)
    (g :: forall c g
      .  (forall e. Iso (g e) (e -> c -> c))
      -> (a -> c -> c)
      -> c
      -> c)
    .
  augmentW g (buildW f) = buildW (\i c n -> g i c (f i c n))
  #-}
