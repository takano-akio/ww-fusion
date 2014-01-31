{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module F where
--import qualified Data.IntMap as IM
import WWFusion as WW

f :: [[Int]] -> Int
f x = foldl' (+) 0 $ WW.concat x

g :: [Int] -> [Int] -> Int
g x y = foldl' (+) 0 $ x WW.++ y

h :: Tree -> Int
h t = WW.foldl (+) 0 $ toList t

data Tree
  = Bin !Tree !Tree
  | Tip {-# UNPACK #-} !Int

toList :: Tree -> [Int]
toList = \tree -> buildW (toListFB tree)
{-# INLINE toList #-}

toListFB
  :: forall r f
  .  Tree
  -> (Wrap f r)
  -> (Int -> r -> r) -> r -> r
toListFB root (Wrap wrap unwrap) c n = wrap go root n
  where
    go :: f Tree
    go = unwrap $ \t z -> case t of
      Bin l r -> wrap go l (wrap go r z)
      Tip x -> c x z
{-# INLINE [0] toListFB #-}
