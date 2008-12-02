----------------------------------------------
-- |
-- Module    : Control.Monad.Omega
-- Copyright : (c) Luke Palmer 2008
-- License   : Public Domain
--
-- Maintainer : Luke Palmer <lrpalmer@gmail.com>
-- Stability : experimental
-- Portability : portable
--
-- A monad for enumerating sets: like the list monad, but
-- impervious to infinite descent.
--
-- A depth-first search of a data structure can fail to give a full traversal
-- if it has an infinitely deep path.  Likewise, a breadth-first search of a
-- data structure can fall short if it has an infinitely branching node.
-- Omega addresses this problem by using a \"diagonal\" traversal
-- that gracefully dissolves such data.
--
-- So while @liftM2 (,) [0..] [0..]@ gets \"stuck\" generating tuples whose
-- first element is zero, @"runOmega" $ liftM2 (,) ("each" [0..]) ("each"
-- [0..])@ generates all pairs of naturals.
--
-- More precisely, if @x@ appears at a finite index in
-- @xs@, and @y@ appears at a finite index in @f x@,
-- then @y@ will appear at a finite index in @each xs >>= f@. 
--
-- This monad gets its name because it is a monad over sets of order type
-- omega.
--
-- Warning: Omega is only a monad when the results of @runOmega@ are
-- interpreted as a set; that is, a valid transformation according to the
-- monad laws may change the order of the results.  However, the same
-- set of results will always be reachable. 
----------------------------------------------

module Control.Monad.Omega 
    (diagonal, Omega, runOmega, each) 
where

import qualified Control.Monad as Monad
import qualified Control.Applicative as Applicative
import qualified Data.Foldable as Foldable
import qualified Data.Traversable as Traversable

-- | This is the hinge algorithm of the Omega monad,
-- exposed because it can be useful on its own.  Joins 
-- a list of lists with the property that for every i j 
-- there is an n such that @xs !! i !! j == diagonal xs !! n@.
-- In particular, @n <= (i+j)*(i+j+1)/2 + j@.
diagonal :: [[a]] -> [a]
diagonal = concat . stripe
    where
    stripe [] = []
    stripe ([]:xss) = stripe xss
    stripe ((x:xs):xss) = [x] : zipCons xs (stripe xss)

    zipCons [] ys = ys
    zipCons xs [] = map (:[]) xs
    zipCons (x:xs) (y:ys) = (x:y) : zipCons xs ys

newtype Omega a = Omega { runOmega :: [a] }

each :: [a] -> Omega a
each = Omega

instance Functor Omega where
    fmap f (Omega xs) = Omega (map f xs)

instance Monad Omega where
    return x = Omega [x]
    Omega m >>= f = Omega $ diagonal $ map (runOmega . f) m
    fail _ = Omega []

instance Monad.MonadPlus Omega where
    mzero = Omega []
    mplus (Omega xs) (Omega ys) = Omega (diagonal [xs,ys])

instance Applicative.Applicative Omega where
    pure = return
    (<*>) = Monad.ap

instance Foldable.Foldable Omega where
    foldMap f (Omega xs) = Foldable.foldMap f xs

instance Traversable.Traversable Omega where
    traverse f (Omega xs) = fmap Omega $ Traversable.traverse f xs
