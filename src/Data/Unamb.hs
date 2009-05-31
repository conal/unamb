{-# LANGUAGE ScopedTypeVariables, RecursiveDo, CPP, DeriveDataTypeable #-}
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
----------------------------------------------------------------------

-- #include "Typeable.h"

module Data.Unamb
  (
    -- * Purely functional unambiguous choice
    unamb
    -- * Some useful special applications of 'unamb'
  , unambs, assuming, asAgree
  , parCommute, parIdentity, parAnnihilator
  , por, pand, pmin, pmax, pmult
    -- * Some related imperative tools
  , amb, race
    -- * Exception thrown if neither value evaluates
  , BothBottom
  ) where

import Prelude hiding (catch)
import System.IO.Unsafe
import Data.Function (on)
import Control.Monad.Instances () -- for function functor
import Control.Concurrent
import Control.Exception
import Data.Typeable

--import Data.IsEvaluated

-- Use a particular exception as our representation for waiting forever.
data BothBottom = BothBottom deriving(Show,Typeable)

instance Exception BothBottom

-- | Unambiguous choice operator.  Equivalent to the ambiguous choice
-- operator, but with arguments restricted to be equal where not bottom,
-- so that the choice doesn't matter.  See also 'amb'.
--
-- If anything kills unamb while it is evaluating (like nested unambs), it can
-- be retried later but, unlike most functions, work may be lost.
unamb :: a -> a -> a
unamb a b = unsafePerformIO (retry (amb a b))
    where retry act = act `catch`
                      (\(SomeException e) -> do
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
                          myThreadId >>= flip throwTo e
                          unblock $ retry act)


-- | n-ary 'unamb'
unambs :: [a] -> a
unambs []  = undefined
unambs [x] = x
unambs xs  = foldr unamb undefined xs

-- | Ambiguous choice operator.  Yield either value.  Evaluates in
-- separate threads and picks whichever finishes first.  See also
-- 'unamb' and 'race'.
amb :: a -> a -> IO a
amb a b = do 
  -- First, check whether one of the values already is evaluated
  -- #ifdef this out for non-GHC code.
  a' <- return False --isEvaluated a
  b' <- return False --isEvaluated b
  case (a',b') of
    (True,_) -> return a
    (_,True) -> return b
    _        -> race (evaluate a) (evaluate b)

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
--                       do throwIO e
--                          myThreadId >>= killThread
--                          unblock (race a b)
--                     _ -> throwIO e


-- Finally, an improved version written by Svein Ove Aas

-- This version kills descendant threads when killed, but does not restart
-- any work if it's called by unamb. That code is left in unamb.

race a b = block $ do
  v <- newEmptyMVar
  let f x = forkIO $ putCatch x v
  ta <- f a
  tb <- f b
  let cleanup = killThread ta >> killThread tb
      loop 0 = throwIO BothBottom
      loop t = do x <- takeMVar v
                  case x of Nothing -> loop (t-1)
                            Just x' -> return x'
  unblock (loop (2 :: Int) `finally` cleanup)
  

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
                 -- This next handler hides bogus black holes, which show up as
                 -- "<<loop>>" messages.  I'd rather eliminate the problem than hide it.
                 -- TODO: Remove and stress-test (e.g., reactive-fieldtrip)
                 , Handler $ \ NonTermination    -> print "Unamb.hs: Bogus black hole?" >> throwIO NonTermination
                 ]


-- | Yield a value if a condition is true.  Otherwise undefined.
assuming :: Bool -> a -> a
assuming True  a = a
assuming False _ = undefined

-- | The value of agreeing values (or undefined/bottom)
asAgree :: Eq a => a -> a -> a
a `asAgree` b = assuming (a == b) a


{--------------------------------------------------------------------
    Some useful special applications of 'unamb'
--------------------------------------------------------------------}

-- | Turn a binary commutative operation into that tries both orders in
-- parallel.  Useful when there are special cases that don't require
-- evaluating both arguments.  For non-flat types and information merging,
-- see @parCommute@ in the @lub@ package.
parCommute :: (a -> a -> b) -> (a -> a -> b)
parCommute op x y = (x `op` y) `unamb` (y `op` x)

-- | Parallel or
por :: Bool -> Bool -> Bool
por = parCommute (||)

-- | Parallel and
pand :: Bool -> Bool -> Bool
pand = parCommute (&&)


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
parAnnihilator op ann x y =
  assuming (x == ann) ann `unamb`
  assuming (y == ann) ann `unamb`
  (x `op` y)

-- | Binary operation with left & right identity element.  For instance, (*) & 1,
-- (&&) & 'True', (||) & 'False', 'min' & 'maxBound', 'max' & 'minBound'.
-- Tests either argument as identity, in parallel.
parIdentity :: (Eq a) => (a -> a -> a) -> a -> a -> a -> a
parIdentity op ident x y =
  assuming (x == ident) y `unamb`
  assuming (y == ident) x `unamb`
  (x `op` y)


-- | Parallel min with minBound short-circuit
pmin :: (Ord a, Bounded a) => a -> a -> a
pmin = parAnnihilator min minBound

-- | Parallel max with maxBound short-circuit
pmax :: (Ord a, Bounded a) => a -> a -> a
pmax = parAnnihilator max maxBound

-- | Parallel multiplication with 0 short-circuit
pmult :: Num a => a -> a -> a
pmult = parAnnihilator (*) 0

{-

-- Examples:

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
