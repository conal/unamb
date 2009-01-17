{-# OPTIONS_GHC -Wall #-}

-- TestRace.hs.  Compile & run:
--   ghc --make -threaded TestRace.hs
--   ./TestRace +RTS -N2

-- Prints consecutive natural numbers for a while, and then gets stuck.
-- Seems to run fine without -threaded/N2

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


-- unamb :: a -> a -> a
-- a `unamb` b = unsafePerformIO (evaluate a `race` evaluate b)

-- -- | Race two actions against each other in separate threads, and pick
-- -- whichever finishes first.  See also 'amb'.
-- race :: IO a -> IO a -> IO a

-- -- Old version.  Doesn't take care to recursively terminate descendent
-- -- threads.  This version seems to work forever, but with degraded
-- -- performance.  I guess there are more & more useless live threads.
-- a `race` b = do v  <- newEmptyMVar
--                 let f x = forkIO (putCatch x v)
--                 ta <- f a
--                 tb <- f b
--                 x  <- takeMVar  v
--                 killThread ta
--                 killThread tb
--                 return x


-- -- New version, based on suggestions from Sterling Clover and Bertram
-- -- Felgenhauer.  This one seems to work correctly and perform well, unless
-- -- compiled with -threaded and run with +RTS -N2, which leads to lock-up
-- -- after a while.
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
--                       -- kill self synchronously and then retry if
--                       -- evaluated again.
--                       do myThreadId >>= killThread
--                          unblock (race a b)
--                     _ -> throwIO e


-- This one locks up after a while even in ghci.  When compiled -threaded
-- and run +RTS -N2, it locks up almost immediately.

-- a `race` b = do
--    v <- newEmptyMVar
--    let t x = x >>= putMVar v
--    withThread (t a) $ withThread (t b) $ takeMVar v
--  where
--   withThread u v = brackAsync (forkIO u) killThread (const v)

-- brackAsync :: IO t -> (t -> IO b) -> (t -> IO a) -> IO a
-- brackAsync before after thing =
--   block $ do
--     a <- before
--     catch  (unblock (thing a) >>= \r -> after a >> return r) $
--            \e -> do
--                   after a
--                   myThreadId >>= flip throwTo (e :: SomeException)
--                   brackAsync before after thing




-- putCatch :: IO a -> MVar a -> IO ()
-- putCatch act v = (act >>= putMVar v) `catches` 
--   [ Handler $ \ (ErrorCall _)     -> return ()
--   , Handler $ \ BlockedOnDeadMVar -> return ()
--   -- This next handler hides bogus black holes, which show up as
--   -- "<<loop>>" messages.  I'd rather eliminate the problem than hide it.
--   -- , Handler $ \ NonTermination    -> return ()
--   ]


-- Luke Palmer's stress test
sparse :: ()
sparse =
  foldr1 unamb [ if x == (10000 :: Integer) then () else blockForever | x <- [0..] ]
 where
   blockForever = unsafePerformIO $
                  newEmptyMVar >>= takeMVar
