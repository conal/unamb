{-# OPTIONS_GHC -Wall #-}
----------------------------------------------------------------------
-- |
-- Module      :  Main
-- Copyright   :  (c) Conal Elliott 2008
-- License     :  BSD3
-- 
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
-- 
-- Unamb tests
----------------------------------------------------------------------

module Main where

import Test.QuickCheck.Checkers
import Test.QuickCheck.Later

import Data.Unamb

import System.IO.Unsafe
import Control.Concurrent

main :: IO ()
main = quickBatch batch


batch :: TestBatch
batch = ( "FRP.Reactive.Unamb"
        , [ ("both identity", bothId                unambt undefined)
          , ("idempotence"  , idempotent2           unambt)
          , ("commutative"  , isCommutTimes 0.00001 unambt)
          , ("associative"  , isAssocTimes  0.00001 unambt)
          -- These still get tested 500 times. Pointless, but the best I can do on short order. - Svein
          -- TODO: Replace the A/b and B/a tests with something simpler.  At the
          -- very least, add a comment here.
          , ("recursive A"  , eq b 30)
          , ("recursive B"  , eq a 42)
          ]
        )
 where
   -- monomorphic test version
   unambt :: NumT -> NumT -> NumT
   unambt = unamb
   -- For the recursive tests
   x = unsafePerformIO (threadDelay 100000 >> return (42::Int))
   a = unamb x undefined
   b = unamb 30 a

-- On Windows the commutative and associative test take a long time
-- because of the intentional delays.  I don't understand the magnitude of
-- the delays, however.  They appear to be 1000 times what I'd expect.
-- For instance, 0.00001 sec time 500 tests is 5 milliseconds, but I count
-- about 10 seconds.
-- 
-- On Linux, everything is zippy.

