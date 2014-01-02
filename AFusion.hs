{-# LANGUAGE RankNTypes #-}
module AFusion 
  ( foldrW
  , buildW
  , foldl'
  , foldr
  , (++)
  , concat
  ) where

import Prelude hiding ((++), foldr, concat)

foldrW
  :: (forall e. f e -> e -> b -> b)
  -> (forall e. (e -> b -> b) -> f e)
  -> (a -> b -> b)
  -> b
  -> [a]
  -> b
foldrW wrap unwrap f z0 list0 = wrap go list0 z0
  where
    go = unwrap $ \list z' -> case list of
      [] -> z'
      x:xs -> f x $ wrap go xs z'
{-# NOINLINE[0] foldrW #-}

newtype Simple b e = Simple { runSimple :: e -> b -> b }

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f z = foldrW runSimple Simple f z
{-# INLINE foldr #-}

buildW
  :: (forall b f
    .  (forall e. f e -> e -> b -> b)
    -> (forall e. (e -> b -> b) -> f e)
    -> (a -> b -> b)
    -> b
    -> b)
  -> [a]
buildW g = g runSimple Simple (:) []
{-# INLINE[0] buildW #-}

augmentW
  :: (forall b f
    .  (forall e. f e -> e -> b -> b)
    -> (forall e. (e -> b -> b) -> f e)
    -> (a -> b -> b)
    -> b
    -> b)
  -> [a]
  -> [a]
augmentW g xs = g runSimple Simple (:) xs
{-# INLINE[0] augmentW #-}

(++) :: [a] -> [a] -> [a]
a ++ b = augmentW (\w u c n -> foldrW w u c n a) b
{-# INLINE (++) #-}

concat :: [[a]] -> [a]
concat xs = buildW (\w u c n -> foldrW w u (\x y -> foldrW w u c y x) n xs)
{-# INLINE concat #-}

foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' f initial xs = foldrW wrap unwrap g id xs initial
  where
    wrap (Simple s) e k a = k $ s e a
    unwrap u = Simple $ \e -> u e id
    g x next acc = next $! f acc x
{-# INLINE foldl' #-}

{-# RULES
"foldrW/buildW" forall
    f z
    (wrap :: forall e. f e -> e -> b -> b)
    (unwrap :: (forall e. (e -> b -> b) -> f e))
    (g :: forall c g
      .  (forall e. g e -> e -> c -> c)
      -> (forall e. (e -> c -> c) -> g e)
      -> (a -> c -> c)
      -> c
      -> c)
    .
  foldrW wrap unwrap f z (buildW g) = g wrap unwrap f z
"foldrW/augmentW" forall
    f z
    (wrap :: forall e. f e -> e -> b -> b)
    (unwrap :: (forall e. (e -> b -> b) -> f e))
    (g :: forall c g
      .  (forall e. g e -> e -> c -> c)
      -> (forall e. (e -> c -> c) -> g e)
      -> (a -> c -> c)
      -> c
      -> c)
    xs
    .
  foldrW wrap unwrap f z (augmentW g xs) = g wrap unwrap f (foldrW wrap unwrap f z xs)
"augmentW/buildW" forall
    (f :: forall c g
      .  (forall e. g e -> e -> c -> c)
      -> (forall e. (e -> c -> c) -> g e)
      -> (a -> c -> c)
      -> c
      -> c)
    (g :: forall c g
      .  (forall e. g e -> e -> c -> c)
      -> (forall e. (e -> c -> c) -> g e)
      -> (a -> c -> c)
      -> c
      -> c)
    .
  augmentW g (buildW f) = buildW (\w u c n -> g w u c (f w u c n))
  #-}
