{-# LANGUAGE ScopedTypeVariables, RecursiveDo, CPP #-}
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
-- '''lub''' package.
----------------------------------------------------------------------

-- #include "Typeable.h"

module Data.Unamb
  (
    unamb, assuming, asAgree
  , amb, race
  -- * Some useful special applications of 'amb'
  , parCommute, parIdentity, parAnnihilator
  , por, pand, pmin, pmax, pmult
  ) where

import Prelude hiding (catch)
import System.IO.Unsafe
import Data.Function (on)
import Control.Monad.Instances () -- for function functor
import Control.Concurrent
import Control.Exception


-- | Unambiguous choice operator.  Equivalent to the ambiguous choice
-- operator, but with arguments restricted to be equal where not bottom,
-- so that the choice doesn't matter.  See also 'amb'.
unamb :: a -> a -> a
unamb = (fmap.fmap) unsafePerformIO amb

-- a `unamb` b = unsafePerformIO (a `amb` b)

-- | Ambiguous choice operator.  Yield either value.  Evaluates in
-- separate threads and picks whichever finishes first.  See also
-- 'unamb' and 'race'.
amb :: a -> a -> IO a
amb = race `on` evaluate

-- a `amb` b = evaluate a `race` evaluate b

-- | Race two actions against each other in separate threads, and pick
-- whichever finishes first.  See also 'amb'.
race :: IO a -> IO a -> IO a

-- a `race` b = do v  <- newEmptyMVar
--                 let f x = forkIO (putCatch x v)
--                 ta <- f a
--                 tb <- f b
--                 x  <- takeMVar  v
--                 killThread ta
--                 killThread tb
--                 return x

-- a `race` b = do v  <- newEmptyMVar
--                 let f x = forkIO (putCatch x v)
--                 ta <- f a
--                 tb <- f b
--                 let kill = killThread ta >> killThread tb
--                 block (do
--                   x <- (takeMVar v) `onException` kill
--                   kill
--                   return x)

-- Based on suggestions from Sterling Clover and Bertram Felgenhauer
race a b = block $ do
   v <- newEmptyMVar
   let f x = forkIO (unblock (putCatch x v))
   ta <- f a
   tb <- f b
   let cleanup = killThread ta >> killThread tb
   (do r <- takeMVar v; cleanup; return r) `catch`
       \e -> do cleanup
                case fromException e of
                    Just ThreadKilled ->
                      -- kill self synchronously and then retry if
                      -- evaluated again.
                      do myThreadId >>= killThread
                         unblock (race a b)
                    _ -> throwIO e

{-
forkIO' :: IO () -> IO ThreadId
forkIO' io = forkIO (do myThreadId >>= print'
                        io)

putStrLn' :: String -> IO ()
putStrLn' = locking . putStrLn

print' :: Show a => a -> IO ()
print' = putStrLn' . show

theLock :: MVar ()
theLock = unsafePerformIO (newMVar ())

locking :: IO a -> IO ()
locking io = do () <- takeMVar theLock
                io
                putMVar theLock ()
-}

-- forkPut :: IO a -> MVar a -> IO ThreadId
-- forkPut = (fmap.fmap) forkIO putCatch


-- a `race` b = do v <- newEmptyMVar
--                 ta <- forkPut a v
--                 (do tb <- forkPut b v
--                     takeMVar v `finally` killThread tb)
--                  `finally` killThread ta



-- a `race` b = do v <- newEmptyMVar
--                 forking (putCatch a v) $
--                   forking (putCatch b v) $
--                     takeMVar v

{-
-- | Fork a thread, then do an action.  Finally, kill the forked thread,
-- even in case of an exception, such as the parent thread being killed
forking :: IO () -> IO a -> IO a

-- forking a b = bracket (forkIO a) killThread (const b)

-- Thanks to Spencer Janssen for the forking implementation above, which
-- is safer than my old version:

forking act k = do tid <- forkIO act
                   k `finally` killThread tid

-- Strangely, some of my unamb uses in reactive don't work with Spencer's
-- version.
-}


-- Use a particular exception as our representation for waiting forever.
-- A thread can bottom-out efficiently by throwing that exception.  If both
-- threads bail out, then the 'takeMVar' would block.  In that case, the
-- run-time system would notice and raise 'BlockedOnDeadMVar'.  I'd then
-- want to convert that exception into the one that wait-forever
-- exception.  As an expedient hack, I use 'BlockedOnDeadMVar' as the
-- wait-forever exception, so that no conversion is needed.  Perhaps
-- revisit this choice, and define our own exception class, for clarity
-- and easier debugging.


-- Execute a given action and store the result in an MVar.  Catch
-- 'error' calls, bypassing the MVar write.  Two racing two aborted threads
-- in this way can result in 'BlockedOnDeadMVar', so catch that exception
-- also.
putCatch :: IO a -> MVar a -> IO ()
putCatch act v = (act >>= putMVar v) `catches` 
  [ Handler $ \ (ErrorCall _)     -> return ()
  , Handler $ \ BlockedOnDeadMVar -> return ()
  -- This next handler hides bogus black holes, which show up as
  -- "<<loop>>" messages.  I'd rather eliminate the problem than hide it.
  -- , Handler $ \ NonTermination    -> return ()
  ]


-- putCatch act v = (act >>= putMVar v) `catch` uhandler `catch` bhandler
--  where
--    uhandler (ErrorCall _)     = return ()
--    bhandler BlockedOnDeadMVar = return ()


-- -- Fork a thread to execute a given action and store the result in an
-- -- MVar.  Catch 'undefined', bypassing the MVar write.  Two racing two
-- -- aborted threads in this way can result in 'BlockedOnDeadMVar', so catch
-- -- that exception also.
-- forkPut :: IO a -> MVar a -> IO ThreadId
-- forkPut act v = forkIO ((act >>= putMVar v) `catch` uhandler `catch` bhandler)
--  where
--    uhandler (ErrorCall "Prelude.undefined") = return ()
--    uhandler err                             = throw err
--    bhandler BlockedOnDeadMVar               = return ()

-- | Yield a value if a condition is true.  Otherwise wait forever.
assuming :: Bool -> a -> a
assuming True  a = a
assuming False _ = undefined

-- | The value of agreeing values (or hang)
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

-- | Binary operation with annihilator element.  For instance, '(*)'/0,
-- '(&&)'/'False', '(||)'/'True', 'min'/'minBound', 'max'/'maxBound'.
-- Tests either argument as annihilator, in parallel.
parAnnihilator :: Eq a => (a -> a -> a) -> a -> (a -> a -> a)
parAnnihilator op ann x y =
  assuming (x == ann) ann `unamb`
  assuming (y == ann) ann `unamb`
  (x `op` y)

-- | Binary operation with left & right identity element.  For instance, '(*)'/1,
-- '(&&)'/'True', '(||)'/'False', 'min'/'maxBound', 'max'/'minBound'.
-- Tests either argument as identity, in parallel.
parIdentity :: (Eq a) => (a -> a -> a) -> a -> a -> a -> a
parIdentity op ident x y =
  assuming (x == ident) y `unamb`
  assuming (y == ident) x `unamb`
  (x `op` y)


-- | Parallel min with minBound short-circuit
pmin :: (Ord a, Bounded a) => a -> a -> a
pmin = parAnnihilator min minBound

-- | Parallel max with minBound short-circuit
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
