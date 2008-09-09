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

main :: IO ()
main = quickBatch batch


batch :: TestBatch
batch = ( "FRP.Reactive.Unamb"
        , [ ("both identity", bothId                unambt hang)
          , ("idempotence"  , idempotent2           unambt)
          , ("commutative"  , isCommutTimes 0.00001 unambt)
          , ("associative"  , isAssocTimes  0.00001 unambt)
          ]
        )
 where
   -- monomorphic test version
   unambt :: NumT -> NumT -> NumT
   unambt = unamb

-- The commutative and associative test take a long time because of the
-- intentional delays.  I don't understand the magnitude of the delays,
-- however.  They appear to be 1000 times what I'd expect.  For instance,
-- 0.00001 sec time 500 tests is 5 milliseconds, but I count about 10
-- seconds.
