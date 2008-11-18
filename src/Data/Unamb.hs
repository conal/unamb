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
----------------------------------------------------------------------

#include "Typeable.h"

module Data.Unamb
  (
    bottom, unamb, assuming, asAgree, hang
  , amb, race
  ) where

import Prelude hiding (catch)
-- For hang
-- import Control.Monad (forever)
import System.IO.Unsafe

-- import Data.Dynamic

import Control.Concurrent
import Control.Exception
  (evaluate, BlockedOnDeadMVar(..), catch, throw)


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
-- whichever finishes first.  See also 'amb'.  Thanks to Spencer Janssen
-- for this simple version.
race :: IO a -> IO a -> IO a

a `race` b = do v  <- newEmptyMVar
                ta <- forkIO' (a >>= putMVar v)
                tb <- forkIO' (b >>= putMVar v)
                x  <- takeMVar v
                killThread ta
                killThread tb
                return x

-- Use a particular exception as our representation for waiting forever.
-- A thread can bottom-out efficiently by throwing that exception.  If both
-- threads bail out, then the 'takeMVar' would block.  In that case, the
-- run-time system would notice and raise 'BlockedOnDeadMVar'.  I'd then
-- want to convert that exception into the one that wait-forever
-- exception.  As an expedient hack, I use 'BlockedOnDeadMVar' as the
-- wait-forever exception, so that no conversion is needed.  Perhaps
-- revisit this choice, and define our own exception class, for clarity
-- and easier debugging.


-- Fork a thread to execute a given action.  Silence any raised exceptions.
forkIO' :: IO () -> IO ThreadId
forkIO' act = forkIO (act `catch` handler)
 where
   handler :: BlockedOnDeadMVar -> IO ()
   handler = const (return ())

-- I'd like @hang `unamb` hang@ to quickly terminate, throwing an
-- exception.  I'm surprised that it doesn't lead to 'BlockedOnDeadMVar'.
-- Why doesn't it??  Oh -- maybe it does, when compiled.


-- | A 'bottom' value, allowing no information out.  A left- and right-
-- identity for 'unamb'.  At the top level, evaluating 'bottom' results in
-- the message "Exception: thread blocked indefinitely".
bottom :: a
bottom = throw BlockedOnDeadMVar

-- {-# DEPRECATED hang "use bottom instead" #-}

-- | Never yield an answer.  Like 'undefined' or 'error "whatever"', but
-- don't raise an error, and don't consume computational resources.
hang :: a
hang = bottom

-- | Yield a value if a condition is true.  Otherwise wait forever.
assuming :: Bool -> a -> a
assuming c a = if c then a else hang

-- | The value of agreeing values (or hang)
asAgree :: Eq a => a -> a -> a
a `asAgree` b = assuming (a == b) a

----

{-

data WaitForever = WaitForever

INSTANCE_TYPEABLE0(WaitForever,waitForeverTc,"WaitForever")

instance Show WaitForever where
    showsPrec _ WaitForever = showString "waiting for, like, evar"
instance Exception WaitForever

-}

----

{--------------------------------------------------------------------
    Some useful special applications of 'unamb'
--------------------------------------------------------------------}

-- | Parallel or
por :: Bool -> Bool -> Bool
por = parCommute (||)

-- | Parallel and
pand :: Bool -> Bool -> Bool
pand = parCommute (&&)

-- | Addition optimized for either argument being zero, where the other
-- might be expensive/delayed.
pplus :: Num a => a -> a -> a
pplus = parCommute plus
 where
   0 `plus` b = b
   a `plus` b = a+b

-- | Multiplication optimized for either argument being zero or one, where
-- the other might be expensive/delayed.
ptimes :: Num a => a -> a -> a
ptimes = parCommute times
 where
   0 `times` _ = 0
   1 `times` b = b
   a `times` b = a*b

-- | Turn a binary commutative operation into that tries both orders in
-- parallel.
parCommute :: (a -> a -> a) -> (a -> a -> a)
parCommute op a b = (a `op` b) `unamb` (b `op` a)
