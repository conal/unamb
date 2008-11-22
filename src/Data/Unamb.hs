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
  , parCommute, por, pand
  ) where

import Prelude hiding (catch)
import System.IO.Unsafe
import Data.Function (on)
import Control.Monad.Instances () -- for function functor
import Control.Concurrent
import Control.Exception
  (evaluate, ErrorCall(..), BlockedOnDeadMVar(..), catch, throw)


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
a `race` b = do v  <- newEmptyMVar
                ta <- forkPut a v
                tb <- forkPut b v
                x  <- takeMVar  v
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


-- Fork a thread to execute a given action and store the result in an
-- MVar.  Catch 'undefined', bypassing the MVar write.  Two racing two
-- aborted threads in this way can result in 'BlockedOnDeadMVar', so catch
-- that exception also.
forkPut :: IO a -> MVar a -> IO ThreadId
forkPut act v = forkIO ((act >>= putMVar v) `catch` uhandler `catch` bhandler)
 where
   uhandler (ErrorCall "Prelude.undefined") = return ()
   uhandler err                             = throw err
   bhandler BlockedOnDeadMVar               = return ()

-- | Yield a value if a condition is true.  Otherwise wait forever.
assuming :: Bool -> a -> a
assuming c a = if c then a else undefined

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

a `por` b = (a || b) `unamb` (b || a)


-- por = parCommute (||)

-- | Parallel and
pand :: Bool -> Bool -> Bool
pand = parCommute (&&)

{-

-- Examples:

undefined `por` True
True `por` undefined

undefined `pand` False
False `pand` undefined

-}

