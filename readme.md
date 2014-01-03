ww-fusion
=========

These files demonstrate an idea about extending foldr
to allow the user to specify how the recursion
is encoded.

Motivation
----------

Suppose you want to define foldl' in terms of foldr. The standard
way to do this is:

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

The idea is to define an extended version of foldr:

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

Discussion
----------

* Wouldn't a better arity analyzer solve this?
    * It would work in simpler cases, but I don't think it would be a general
       solution. In particular, when foldl' consumes a list that was produced
       by a tree-traversal function, I believe we really need
       to un-CPS-transform the loop to eliminate allocation.
