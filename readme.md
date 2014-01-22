ww-fusion
=========

These files demonstrate an idea about extending foldr
to allow the user to specify how the recursion
is encoded.

Motivation
----------

The goal is to make foldl' (and related functions like foldM)
fuse well with other list functions.
This would make fold/build fusion more powerful by eliminating
many more intemediate lists, because it's quite common that
the final consumer of a list is an application of foldl'.

Background
----------

Currently foldl' is not a good consumer. To turn it a good consumer,
we'd need to define it in terms of foldr. The problem is that such
an encoding doesn't result in efficient code, at least under the
current GHC optimizer.

The standard way to define foldl' in terms of foldr is:

    foldl' :: (b -> a -> b) -> b -> [a] -> b
    foldl' f v xs = foldr g id xs v
      where
        g x next acc = next $! f x acc

This definition works, but it doesn't result in efficient code. GHC
generates something essentially like:

    foldl' :: (b -> a -> b) -> b -> [a] -> b
    foldl' f v xs = let
        go [] = id
        go (x:xs) = (go xs $!) . f x
      in go xs acc

The problem here is that `go` is defined as a recursive function that
creates a chain of closures, which then are used only once. Note
that this allocates memory, whereas the standard definition of
foldl' doesn't need any allocation (apart from what `f` allocates).

Idea
----

The idea is that instead of defining list operations in terms of foldr
and build, use a slight generalization of them, namely foldrW and buildW.
The definition of foldrW looks like:

    -- | A mapping between @a@ and @b@.
    data Wrap a b = Wrap (a -> b) (b -> a)

    foldrW
      :: (forall e. Wrap (f e) (e -> b -> b))
      -> (a -> b -> b) -> b -> [a] -> b
    foldrW (Wrap wrap unwrap) f z0 list0 = wrap go list0 z0
      where
        go = unwrap $ \list z' -> case list of
          [] -> z'
          x:xs -> f x $ wrap go xs z'

`foldrW` differs from `foldr` in that it takes one extra argument.
That argument defines a mapping between a function type `e -> b -> b`
and a user-specified type `f e`. `foldrW` recursively defines a
value of type `f [a]`, effectively letting the user choose the representation
of the loop.

Note that this is an example of worker-wrapper transformation.

The corresponding build function is defined thus:

    newtype Simple b e = Simple { runSimple :: e -> b -> b }

    buildW
      :: (forall f r.
            (forall e. Wrap (f e) (e -> r -> r))
          -> (a -> r -> r)
          -> r)
      -> [a]
    buildW f = f (Wrap runSimple Simple) (:) []

Using `foldrW`, `foldl'` can be defined as:

    newtype Simple b e = Simple { runSimple :: e -> b -> b }

    foldl' f initial xs = foldrW (Wrap wrap unwrap) g id xs initial
      where
        wrap (Simple s) e k a = k $ s e a
        unwrap u = Simple $ \e -> u e id
        g x next acc = next $! f acc x

The effect of the worker-wrapper split here is to transform a
continuation-passing recursion into a direct-style loop.

More
----

I believe `foldrW` is expressive enough that it can be used to
efficiently define other types
of list consumers, e.g. `mapM_` for the IO monad. I haven't implemented
it yet, though.

The idea of having a versatile fold primitive is generally useful,
not just in conjuction with list fusion. `foldrW` (or its variant
based on `foldMap`) would be useful in context of other container types,
e.g. maps, vectors and byte strings.

Is eta-expansion enough?
------------------------

It is known that the above encoding of `foldl'` in terms of `foldr` can
be made allocation-free by eta-expanding the local function `go`. So
one could argue that, if GHC can automatically eta-expand it, unmodified
fold/build fusion will be able to fuse `foldl'` nicely, so there is no
need for foldrW/buildW.

My answer to this question is that eta-expansion alone is not enough for
two reasons:

* If the initial list producer is a function that traverses a tree, the
  resulting fused loop will be in continuation passing style, which
  means a new closure is allocated for each non-leaf node of the input
  tree. More specifically, consider the following function that flattens
  a tree into a list:

        data Tree = Tip {-# UNPACK #-} !Int | Bin Tree Tree

        toList :: Tree -> [Int]
        toList tree = build (toListFB tree)
        {-# INLINE toList #-}

        toListFB :: Tree -> (Int -> r -> r) -> r -> r
        toListFB root cons nil = go root nil
          where
            go (Tip x) rest = cons x rest
            go (Bin x y) rest = go x (go y rest)

    Let's say we want to eliminate the intermediate list in the expression
    `(sum (toList t))`. Currently sum is not a good consumer, but if it
    were, after fusion we'd get something like:

        sumList :: Tree -> Int
        sumList root = go0 root id 0

        go0 :: Tree -> (Int -> Int) -> Int -> Int
        go0 (Tip x) k = (k $!) .  (x+)
        go0 (Bin x y) k = go0 x (go0 y k)

    Now, merely eta-expanding go0 is not enough to get efficient code,
    because the function will still build a partial application every
    time it sees a Bin constructor. For this recursion to work in an
    allocation-free way, it must be rather like:

        go1 :: Tree -> Int -> Int
        go1 (Tip x) n = x + n
        go1 (Bin x y) n = go1 y (go1 x n)

    And this is what we get if we define foldl' and toList in terms of
    `foldrW` and `buildW`.

* GHC would eta-expand a function only when it knows for sure that
  eta-expansion will not duplicate work. For `foldl'` this is not a
  problem, but there are other list consumers for which this analysis
  is not locally possible. For example, consider `mconcat`, specialized
  for bytestring builder:

        mconcat :: [Builder] -> Builder
        mconcat . map fromInt :: [Int] -> Builder

    where fromInt is a functin that serializes a value of Int, and Builder
    is a newtype of some function type. Here, it's often possible to get
    a big performance boost by allowing `fromInt` to be executed every time
    the resulting the builder is called (as a function), rather than just
    once. The user may or may not want this. In cases like this, it would be
    difficult for ghc to generate code the user expects, because (1) it's
    not possible to tell the resulting function is run only once by analyzing
    the code locally, and (2) the user may be willing to actually repeat
    the call to `fromInt`. `foldrW` and `buildW` would give more control
    to the user in such cases.

Will the functions currently fusible continue to fuse well?
-----------------------------------------------------------

I believe basically all of the current good consumer or producer will continue
to fuse well under the new `foldrW`/`buildW` framework, because `foldr` can
be defined in terms of `foldrW` (by passing a trivial wrapper) and `build` can
be defined in terms of `buildW` (by ignoring the given wrapper). However I'll
need to actually replace the whole set of list functions using `foldrW/buildW`
to see if this is true.

As a preliminary evidence, here is a criterion benchmark that compares
`foldrW/buildW` against `fold/build` in a number of example scenarios:

http://htmlpreview.github.io/?https://github.com/takano-akio/ww-fusion/blob/master/fusion.html

Alternatives
------------

There seem to be many ways to extend fold/build fusion to get a similar
expressivity to `foldrW/buildW`. One of them is particularly attractive because
it can be described with just those building blocks that many Haskell
programmers are familiar with.
It uses the following pair of functions in place of foldr and build.

    mapA :: (ArrowApply a) => a b () -> [b] -> a () ()
    buildA :: (forall a. (ArrowApply a) => a b () -> a () ()) -> [b]

However it seems like the GHC optimizer and/or the `Arrow` class need to be
tweaked in order for this framework to work efficiently.
