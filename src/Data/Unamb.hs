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

#include "Typeable.h"

module Data.Unamb
  (
    bottom, unamb, assuming, asAgree, hang
  , amb, race
  -- * Some useful special applications of 'amb'
  , parCommute, por, pand
  ) where

import Prelude hiding (catch)
import System.IO.Unsafe

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

import Data.Dynamic  -- move up

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

{-

-- Examples:

bottom `por` True
True `por` bottom

bottom `pand` False
False `pand` bottom

-}

