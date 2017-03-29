import Control.Exception
import Data.Unamb

fib :: Integer -> Integer
fib n | n < 2 = 1
fib n = fib (n-1) + fib (n-2)

main :: IO ()
main = do
  let v1 = True
      v2 = fib 38 `seq` False
  putStrLn "Start"
  v <- evaluate (unamb v1 v2)
  print v
  putStrLn "End. This should be instantaneous."
