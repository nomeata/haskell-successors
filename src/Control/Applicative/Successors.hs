{- |
Description : Successor Functor
Copyright   : © Joachim Breitner, 2017
License     : MIT
Maintainer  : mail@joachim-breitner.de

A value of type @'Succs' a@ represents a step in a graph with nodes of type @a@, or in other words, a node plus the list of its successors.

The 'Applicative' instance for @'Succs' a@ is such that the node represented by @a '<*>' b@ is the application of the node of @a@ applied to the node of @b@, and (more importantly) the successors modeled by @a '<*>' b@ are the node of @a@ applied to the succesors of @b@ in addition to the succesors of @a@ applied to the node of @b@. This way, at most one step is taken.

>>> (++) <$> Succs "0" ["1","2"] <*> Succs "A" ["B","C"]
Succs "0A" ["1A","2A","0B", "0C"]

>>> skip x = Succs [x] [[]]
>>> getSuccs $ (concat <$> traverse skip "Hello")
["ello","Hllo","Helo","Helo","Hell"]

This applicative functor can be useful to define shrink functions for QuickCheck, using the helper

> shrinkSuccs x = Succs x (shrink x)

as explained in a <http://stackoverflow.com/a/41944525/946226 StackExchange answer.>

-}
module Control.Applicative.Successors where

data Succs a = Succs a [a] deriving (Show, Eq)

instance Functor Succs where
    fmap f (Succs o s) = Succs (f o) (map f s)

instance Applicative Succs where
    pure x = Succs x []
    Succs f fs <*> Succs x xs = Succs (f x) (map ($x) fs ++ map f xs)

-- | Return the represented node
getCurrent :: Succs t -> t
getCurrent (Succs x _) = x

-- | Return the represented successors
getSuccs :: Succs t -> [t]
getSuccs (Succs _ xs) = xs

{- The Applicative laws:

  pure id <*> Succs x xs
= Succs id [] <*> Succs x xs
= Succs (id x) (map ($x) [] ++ map id xs)
= Succs x xs

  pure (.) <*> Succs u us <*> Succs v vs <*> Succs w ws
= Succs (.) [] <*> Succs u us <*> Succs v vs <*> Succs w ws
= Succs (u .) (map (.) us) <*> Succs v vs <*> Succs w ws
= Succs (u . v) (map ($v) (map (.) us) ++ map (u .) vs) <*> Succs w ws
= Succs (u . v) (map (($v).(.)) us ++ map (u .) vs) <*> Succs w ws
= Succs ((u . v) w) (map ($w) (map (($v).(.)) us ++ map (u .) vs) ++ map (u.v) ws)
= Succs ((u . v) w) (map (($w).($v).(.)) us ++ map (($w).(u.)) vs ++ map (u.v) ws)
= Succs (u (v w)) (map (\u -> u (v w)) us ++ map (\v -> u (v w)) vs ++ map (\w -> u (v w)) ws)
= Succs (u (v w)) (map ($(v w)) us ++ map u (map ($w) vs ++ map v ws))
= Succs u us <*> Succs (v w) (map ($w) vs ++ map v ws)
= Succs u us <*> (Succs v vs <*> Succs w ws)

  pure f <*> pure x
= Succs f [] <*> Succs x []
= Succs (f x) []
= pure (f x)

  Succs u us <*> pure y
= Succs u us <*> Succs y []
= Succs (u y) (map ($y) us)
= Succs (($y) u) (map ($y) us)
= Succs ($y) [] <*> Succs u us
= pure ($ y) <*> u


-}
