{-# LANGUAGE ScopedTypeVariables, CPP, DeriveDataTypeable #-}
{-# OPTIONS_GHC -Wall #-}
----------------------------------------------------------------------
-- |
-- Module      :  Data.Unamb
-- Copyright   :  (c) Conal Elliott 2008
-- License     :  BSD3
--
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
--
-- Unambiguous choice
--
-- For non-flat types (where values may be partially defined, rather than
-- necessarily bottom or fully defined) and information merging, see the
-- lub package, <http://haskell.org/haskellwiki/Lub>.
--
-- See unamb.cabal for the list of contributors.
----------------------------------------------------------------------

-- #include "Typeable.h"

module Data.Unamb
  (
    -- * Purely functional unambiguous choice
    unamb, unamb'
    -- * Some useful special applications of 'unamb'
  , unambs, assuming, asAgree
  , parCommute, parCommuteShortCircuit
  , parAnnihilator, parIdentity, parAnnihilatorIdentity
  , por, pand, pmin, pmax, pmult
    -- * Some related imperative tools
  , amb, amb', race
    -- * Exception thrown if neither value evaluates
  , BothBottom
  ) where

import Prelude hiding (catch)
import System.IO.Unsafe
import Control.Monad.Instances () -- for function functor
import Control.Concurrent
import Control.Exception hiding (unblock)
import Data.Typeable

-- Drop the unsafeIsEvaluated optimization for now.  See comments below.

-- import Data.TagBits (unsafeIsEvaluated)
-- import Data.IsEvaluated

-- Temporary def until I know how to detect and handle evaluated-as-bottom values.
unsafeIsEvaluated :: a -> Bool
unsafeIsEvaluated = const False

-- | Use a particular exception as our representation for waiting forever.
data BothBottom = BothBottom deriving(Show,Typeable)

instance Exception BothBottom

-- | And another as our representation for a no-longer-needed value
data DontBother = DontBother deriving(Show,Typeable)

instance Exception DontBother

-- | Unambiguous choice operator.  Equivalent to the ambiguous choice
-- operator, but with arguments restricted to be equal where not bottom,
-- so that the choice doesn't matter.  See also 'amb'.
--
-- If anything kills unamb while it is evaluating (like nested unambs), it can
-- be retried later but, unlike most functions, work may be lost.
unamb :: a -> a -> a
unamb a b
    | unsafeIsEvaluated a = a
    | unsafeIsEvaluated b = b
    | otherwise = unamb' a b
{-# INLINE unamb #-}

-- I want something like the following:

-- unamb a b
--     | unsafeIsEvaluated a = if isBottom a then b else a
--     | unsafeIsEvaluated b = if isBottom b then a else b
--     | otherwise = unamb' a b

-- where isBottom assumes that its argument is recognizably evaluated
-- (unsafeIsEvaluated yields True).  What does an evaluated bottom values
-- look like in the RTS?


-- | For use when we already know that neither argument is already evaluated
unamb' :: a -> a -> a
unamb' = (fmap.fmap) restartingUnsafePerformIO amb'
{-# INLINE unamb' #-}

-- unamb a b = restartingUnsafePerformIO (amb a b)

restartingUnsafePerformIO :: IO a -> a
restartingUnsafePerformIO = unsafePerformIO . retry
 where
   -- Exception handling in unsafePerformIO does not happen like you're
   -- used to in normal code. Specifically:
   --
   -- * If a thread running unsafePerformIO code catches an asynchronous
   --   exception, the stack is unwound until the first matching exception
   --   handler as per normal, but if that unwinds it past the invocation
   --   of the unsafePerformIO thunk, the entire state of the code running
   --   in it is saved for later use. If the thunk is later re-entered, it
   --   "unpauses" the code and it continues from where it stopped.
   -- * If the code throws a normal exception, eg. throw/throwIO/pattern
   --   match failure, etc. past the invocation thunk, the thunk is altered
   --   to immediately throw that same exception if it is ever re-entered.
   --
   -- These are both normally good things for efficiency reasons. It
   -- presents us with a pickle when implementing unamb, however:
   --
   -- * unamb is implemented by calling race, which creates threads that
   --   it kills once it completes, for any reason, including exceptions.
   -- * As invocations of unamb are often recursive, this means that
   --   invocations of unamb are often killed by asynchronous exceptions.
   -- * The normal "unpausing" behavior of unsafePerformIO would have them
   --   keep trying to read a dead MVar, whose writers are now-dead threads.
   --
   -- To fix this, we want to restart the action entirely when we catch an
   -- exception.
   --
   -- We do this by adding this exception handler, which instead of returning
   -- normally retries the action at the end. We do of course want to throw
   -- the exception on; however, we can't use throw/throwIO (as that would
   -- make the thunk record itself as bottom), therefore we use throwTo
   -- instead.
   --
   -- Ensuring that the code doesn't execute the retry before the exception
   -- is propagated, throwTo doesn't return until the exception has been
   -- handled.
   --
   -- Incidentally, all exception handlers run inside an implicit block, and
   -- blocking operations contain an implicit unblock. This ensures that any
   -- further pending exceptions won't mess this scheme up, as they can't be
   -- delivered until after throwTo has been called.
   --
   retry :: IO a -> IO a
   retry act =
     act `catch` \ (SomeException e) -> do
       myThreadId >>= flip throwTo e
       mask_ $ retry act


-- | n-ary 'unamb'
unambs :: [a] -> a
unambs = foldr unamb undefined

{-
unambs []  = undefined
unambs xs  = foldr1 unamb' xs `unamb'` foldr findEvaluated undefined xs
    where
        findEvaluated a b | unsafeIsEvaluated a = a
                          | otherwise = b
-}

-- | Ambiguous choice operator.  Yield either value.  Evaluates in
-- separate threads and picks whichever finishes first.  See also
-- 'unamb' and 'race'.
amb :: a -> a -> IO a
amb a b
    | unsafeIsEvaluated a = return a
    | unsafeIsEvaluated b = return b
    | otherwise = amb' a b
{-# INLINE amb #-}

-- | For use when we already know that neither argument is already evaluated
amb' :: a -> a -> IO a
amb' a b = race (evaluate a) (evaluate b)
{-# INLINE amb' #-}

-- | Race two actions against each other in separate threads, and pick
-- whichever finishes first.  See also 'amb'.
race :: IO a -> IO a -> IO a

-- Simple version:

-- a `race` b = do v  <- newEmptyMVar
--                 let f x = forkIO (putCatch x v)
--                 ta <- f a
--                 tb <- f b
--                 x  <- takeMVar  v
--                 killThread ta
--                 killThread tb
--                 return x

-- The simple version doesn't recursively kill descendent threads when
-- killed, which leads to a lot of wasted work.

-- Here is an improved version, based on suggestions from Sterling Clover
-- and Bertram Felgenhauer.  It takes care to kill children when killed.
-- Importantly, it also sets itself up to be retried if the unamb value is
-- accessed again after its computation is aborted.

-- race a b = block $ do
--    v <- newEmptyMVar
--    let f x = forkIO (unblock (putCatch x v))
--    ta <- f a
--    tb <- f b
--    let cleanup = killThread ta >> killThread tb
--    (do r <- takeMVar v; cleanup; return r) `catch`
--        \e -> do cleanup
--                 case fromException e of
--                     Just ThreadKilled ->
--                       -- kill self asynchronously and then retry if
--                       -- evaluated again.
--                       do myThreadId >>= killThread
--                          unblock (race a b)
--                     _ -> throwIO e


-- Finally, an improved version written by Svein Ove Aas

-- This version kills descendant threads when killed, but does not restart
-- any work if it's called by unamb. That code is left in unamb.

race a b = mask_ $ do
  v <- newEmptyMVar
  let f x = forkIO $ putCatch (mask_ x) v
  ta <- f a
  tb <- f b
  let cleanup = throwTo ta DontBother >> throwTo tb DontBother
      loop 0 = throwIO BothBottom
      loop t = do x <- takeMVar v
                  case x of Nothing -> loop (t-1)
                            Just x' -> return x'
  mask_ (loop (2 :: Int) `finally` cleanup)


-- A thread can bottom-out efficiently by throwing that exception.
-- Before a thread bails out for any reason, it informs race of its bailing out.

-- Execute a given action and store the result in an MVar. Catch
-- all errors, bypassing the MVar write and registering a dead thread in that
-- mvar before passing them on.
-- We suppress error-printing for.. what, exactly? When should we *not* do it?
-- Using old code for now.
putCatch :: IO a -> MVar (Maybe a) -> IO ()
putCatch act v = onException (act >>= putMVar v . Just) (putMVar v Nothing) `catches`
                 [ Handler $ \ ErrorCall         {} -> return ()
                 , Handler $ \ BothBottom        {} -> return ()
                 , Handler $ \ PatternMatchFail  {} -> return ()
                 , Handler $ \ DontBother        {} -> return ()
                 -- This next handler hides bogus black holes, which show up as
                 -- "<<loop>>" messages.  I'd rather eliminate the problem than hide it.
                 -- TODO: Remove and stress-test (e.g., reactive-fieldtrip)
                 , Handler $ \ NonTermination    -> print "Unamb.hs: Bogus black hole?" >> throwIO NonTermination
                 ]


-- | Yield a value if a condition is true.  Otherwise undefined.
assuming :: Bool -> a -> a
assuming True  a = a
assuming False _ = undefined
{-# INLINE assuming #-}

-- | The value of agreeing values (or undefined/bottom)
asAgree :: Eq a => a -> a -> a
a `asAgree` b = assuming (a == b) a
{-# INLINE asAgree #-}

-- Note: asAgree == flatGlb (from Data.Glb in lub package).
-- Examine uses of asAgree,u and consider whether glb is a better fit.

{--------------------------------------------------------------------
    Some useful special applications of 'unamb'
--------------------------------------------------------------------}

-- | Turn a binary commutative operation into one that tries both orders in
-- parallel.  Useful when there are special cases that don't require
-- evaluating both arguments.  For non-flat types and information merging,
-- see @parCommute@ in the @lub@ package.
parCommute :: (a -> a -> b) -> (a -> a -> b)
parCommute op x y = (x `op` y) `unamb` (y `op` x)
{-# INLINE parCommute #-}

-- | Turn a binary commutative operation into one that may try both orders.
-- unlike parCommute, if one argument is already evaluated, the function is
-- tried *only* with that as its first argument and not in both orders. When
-- in doubt, use 'parCommute'.

parCommuteShortCircuit :: (a -> a -> b) -> (a -> a -> b)
parCommuteShortCircuit op x y
  | unsafeIsEvaluated x = x `op` y
  | unsafeIsEvaluated y = y `op` x
  | otherwise = parCommute op x y
{-# INLINE parCommuteShortCircuit #-}

-- | Parallel or
por :: Bool -> Bool -> Bool
por = parCommuteShortCircuit (||)
{-# INLINE por #-}

-- | Parallel and
pand :: Bool -> Bool -> Bool
pand = parCommuteShortCircuit (&&)
{-# INLINE pand #-}

-- parAnnihilator op ann = parCommute op'
--  where
--    op' u v | u == ann  = u
--            | otherwise = op u v

-- The parCommute version can waste work while trying the two orderings.
-- In the following version, one branch tries just one annihilator test.

-- parAnnihilator op ann x y = assuming (x == ann) ann `unamb`
--                             (if y == ann then ann else x `op` y)

--- TODO: This definition may be too strict, as it won't use @op@ unless
--- it can prove @y /= ann@.  A lazier version:

-- | Binary operation with annihilator element.  For instance, (*) & 0,
-- (&&) & 'False', (||) & 'True', 'min' & 'minBound', 'max' & 'maxBound'.
-- Tests either argument as annihilator, in parallel.
parAnnihilator :: Eq a => (a -> a -> a) -> a -> (a -> a -> a)
parAnnihilator op ann x y
  | unsafeIsEvaluated x && x == ann = ann
  | unsafeIsEvaluated y && y == ann = ann
  | otherwise =
  assuming (x == ann) ann `unamb'`
  assuming (y == ann) ann `unamb'`
  (x `op` y)

-- | Binary operation with left & right identity element.  For instance, (*) & 1,
-- (&&) & 'True', (||) & 'False', 'min' & 'maxBound', 'max' & 'minBound'.
-- Tests either argument as identity, in parallel.
parIdentity :: (Eq a) => (a -> a -> a) -> a -> a -> a -> a
parIdentity op ident x y
  | unsafeIsEvaluated x && x == ident = y
  | unsafeIsEvaluated y && y == ident = x
  | otherwise =
  assuming (x == ident) y `unamb'`
  assuming (y == ident) x `unamb'`
  (x `op` y)

parAnnihilatorIdentity :: Eq a => (a -> a -> a) -> a -> a -> a -> a -> a
parAnnihilatorIdentity op ann ident x y
  | knownX && x == ann   = ann
  | knownX && x == ident = y
  | knownY && y == ann   = ann
  | knownY && y == ident = y
  | otherwise =
  assuming (x == ident) y `unamb'`
  assuming (x == ann) ann `unamb'`
  assuming (y == ident) x `unamb'`
  assuming (y == ann) ann `unamb'`
  (x `op` y)
  where
    knownX = unsafeIsEvaluated x
    knownY = unsafeIsEvaluated y

-- | Parallel min with minBound short-circuit and maxBound identity
pmin :: (Ord a, Bounded a) => a -> a -> a
pmin = parAnnihilatorIdentity min minBound maxBound
{-# INLINE pmin #-}

-- | Parallel max with maxBound short-circuit and minBound identity
pmax :: (Ord a, Bounded a) => a -> a -> a
pmax = parAnnihilatorIdentity max maxBound minBound
{-# INLINE pmax #-}

-- | Parallel multiplication with 0 short-circuit, and 1 identity
pmult :: (Eq a, Num a) => a -> a -> a
pmult = parAnnihilatorIdentity (*) 0 1
{-# INLINE pmult #-}

{-

-- Examples:

undefined `unamb` 3 :: Int
3 `unamb` undefined :: Int

undefined `por` True
True `por` undefined

undefined `pand` False
False `pand` undefined

0 `pmult` undefined
undefined `pmult` 0

LT `pmin` undefined
undefined `pmin` LT

test :: Int -> Int
test x = f (f x)
    where f v = (x `unamb` v) `seq` v

main = do mapM_ (print . test) [0..]

-}
