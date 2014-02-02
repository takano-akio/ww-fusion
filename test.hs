{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE MagicHash #-}
module F where
--import qualified Data.IntMap as IM
import WWFusion as WW
import GHC.Exts(Ptr(..), Addr#, RealWorld, State#)
import GHC.IO(IO(..), unIO)
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Array

f :: [[Int]] -> Int
f x = foldl' (+) 0 $ WW.concat x

g :: [Int] -> [Int] -> Int
g x y = foldl' (+) 0 $ x WW.++ y

h :: Tree -> Int
h t = WW.foldl (+) 0 $ toList t

-- | Serialize a tree into a buffer. The buffer is assumed to
-- be large enough.
serializeTree :: Tree -> Ptr () -> IO (Ptr ())
serializeTree t buf = foldIO_Ptr write buf $ toList t
  where
    write p x = poke p' x >> return (castPtr $ advancePtr p' 1)
      where
        p' :: Ptr Int
        p' = castPtr p

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

newtype IOInt e = IOInt (e -> Addr# -> State# RealWorld -> (# State# RealWorld, Addr# #))

-- | A version of foldM, specialized for the IO monad and the
-- accumulator type of Ptr (). It uses unboxed ints to pass around
-- intermediate results.
foldIO_Ptr :: (Ptr () -> a -> IO (Ptr ())) -> Ptr () -> [a] -> IO (Ptr ())
foldIO_Ptr f initial = \xs -> foldrW (Wrap wrap unwrap) g return xs initial
  where
    wrap (IOInt h) e k (Ptr a) = io >>= k
      where
        io = IO $ \s -> case h e a s of
          (# s', a' #) -> (# s', Ptr a' #)
    unwrap u = IOInt $ \e a s -> case unIO (u e return (Ptr a)) s of
      (# s', Ptr a' #) -> (# s', a' #)
    g x next acc = next =<< f acc x
