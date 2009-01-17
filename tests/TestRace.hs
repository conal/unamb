{-# OPTIONS_GHC -Wall #-}

-- TestRace.hs.  Compile & run:
--   ghc --make -threaded TestRace.hs
--   ./TestRace +RTS -N2

-- Before http://hackage.haskell.org/trac/ghc/ticket/2910 was resolved,
-- the mapM_ test printed consecutive natural numbers for a while, and
-- then got stuck.  Seemed to run fine without -threaded/N2

import Prelude hiding (catch)

import Data.Unamb

import System.IO.Unsafe
-- import Control.Monad.Instances () -- for function functor
import Control.Concurrent
-- import Control.Exception

test :: Int -> Int
test x = f (f x) where f v = (x `unamb` v) `seq` v

main :: IO ()
-- main = print sparse

main = mapM_ (print . test) [0..]


-- Luke Palmer's stress test
sparse :: ()
sparse =
  foldr1 unamb [ if x == (10000 :: Integer) then () else blockForever | x <- [0..] ]
 where
   blockForever = unsafePerformIO $
                  newEmptyMVar >>= takeMVar
