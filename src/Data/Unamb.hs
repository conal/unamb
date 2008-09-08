{-# LANGUAGE PatternSignatures #-}
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
----------------------------------------------------------------------

module Data.Unamb
  (
    unamb, amb, race, assuming, hang, asAgree
  -- * Tests
  , batch
  ) where

import Test.QuickCheck.Help
import Test.QuickCheck.Later

-- For hang
import Control.Monad (forever)
import System.IO.Unsafe

-- For unamb
import Control.Concurrent
import Control.Exception (evaluate)


-- | Unambiguous choice operator.  Equivalent to the ambiguous choice
-- operator, but with arguments restricted to be equal where not bottom,
-- so that the choice doesn't matter.  See also 'amb'.
unamb :: a -> a -> a
a `unamb` b = unsafePerformIO (a `amb` b)


-- | Ambiguous choice operator.  Yield either value.  Evaluates in
-- separate threads and picks whichever finishes first.  See also
-- 'unamb' and 'race'.
amb :: a -> a -> IO a
a `amb` b = evaluate a `race` evaluate b

-- | Race two actions against each other in separate threads, and pick
-- whichever finishes first.  See also 'amb'.
{-race :: IO a -> IO a -> IO a
a `race` b = 
  -- Evaluate a and b in concurrent threads.  Whichever thread finishes
  -- first kill the other thread.
  do v    <- newEmptyMVar  -- to hold a or b

     -- Thanks to Luke Palmer for pointing out the problem with
     -- using recursive do notation to pass tids to the threads.
     -- Loop is triggered if the first thread gets to killThread
     -- before the second thread has been started.
     -- Workaround involves creating two MVars to hold the tids.

     mta  <- newEmptyMVar
     mtb  <- newEmptyMVar
     lock <- newEmptyMVar  -- to avoid double-kill
     -- Evaluate one value and kill the other.
     let run io mtid = forkIO $ do x <- io
                                   tid <- takeMVar mtid
                                   putMVar lock ()
                                   -- fork a thread to kill the other
                                   -- if we don't, we may end up blocked
                                   -- waiting for the other thread to die.
                                   forkIO (killThread tid) 
                                   putMVar v x
     ta <- run a mtb
     tb <- run b mta
     putMVar mtb tb
     putMVar mta ta
     readMVar v-}

race :: IO a -> IO a -> IO a
race a b = do
    v <- newEmptyMVar
    ta <- forkIO (a >>= putMVar v)
    tb <- forkIO (b >>= putMVar v)
    x <- takeMVar v
    forkIO (killThread ta >> killThread tb)
    return x

-- Without using unsafePerformIO, is there a way to define a
-- non-terminating but non-erroring pure value that consume very little
-- resources while not terminating?

-- | Never yield an answer.  Like 'undefined' or 'error "whatever"', but
-- don't raise an error, and don't consume computational resources.
hang :: a
hang = unsafePerformIO hangIO

-- | Block forever
hangIO :: IO a
hangIO = do -- putStrLn "warning: blocking forever."
            -- Any never-terminating computation goes here
            -- This one can yield an exception "thread blocked indefinitely"
            -- newEmptyMVar >>= takeMVar
            -- sjanssen suggests this alternative:
            forever $ threadDelay maxBound
            -- forever's return type is (), though it could be fully
            -- polymorphic.  Until it's fixed, I need the following line.
            return undefined


-- | Yield a value if a condition is true.  Otherwise wait forever.
assuming :: Bool -> a -> a
assuming c a = if c then a else hang

-- | The value of agreeing values (or hang)
asAgree :: Eq a => a -> a -> a
a `asAgree` b = assuming (a == b) a



{----------------------------------------------------------
    Tests
----------------------------------------------------------}

batch :: TestBatch
batch = ( "FRP.Reactive.Unamb"
        , [ ("both identity", bothId                unambNumericType hang)
          , ("idempotence"  , idempotent2           unambNumericType)
          , ("commutative"  , isCommutTimes 0.00001 unambNumericType)
          , ("associative"  , isAssocTimes  0.00001 unambNumericType)
          ]
        )
 where
   unambNumericType :: NumericType -> NumericType -> NumericType
   unambNumericType = unamb

-- The commutative and associative test take a long time because of the
-- intentional delays.  I don't understand the magnitude of the delays,
-- however.  They appear to be 1000 times what I'd expect.  For instance,
-- 0.00001 sec time 500 tests is 5 milliseconds, but I count about 5
-- seconds.
