module Main where

import Bool
import Either
import Error
import Nat
import Prelude hiding (Bool (True, False), Either, Left, Right, div, gcd, max, min, not, pred, quot, rem, (&&), (*), (+), (-), (/=), (<), (<=), (==), (>), (>=), (^), (||), lcm)

--Testes criados com uso de IA

main :: IO ()
main = do
  putStrLn "Testing Nat module:"
  putStrLn "---------------------"

  let n1 = S (S (S O))
      n2 = S (S O)
      n3 = S O

  putStrLn $ "n1 = " ++ show n1
  putStrLn $ "n2 = " ++ show n2
  putStrLn $ "n3 = " ++ show n3

  putStrLn $ "n1 + n2 = " ++ show (n1 + n2)
  putStrLn $ "n1 * n2 = " ++ show (n1 * n2)
  putStrLn $ "n2 ^ n3 = " ++ show (n2 ^ n3)

  putStrLn $ "n1 == n2: " ++ show (n1 == n2)
  putStrLn $ "n2 == n3: " ++ show (n2 == n3)

  putStrLn $ "n1 /= n2: " ++ show (n1 /= n2)
  putStrLn $ "n2 /= n3: " ++ show (n2 /= n3)

  putStrLn $ "n1 < n2: " ++ show (n1 < n2)
  putStrLn $ "n2 < n3: " ++ show (n2 < n3)

  putStrLn $ "n1 <= n2: " ++ show (n1 <= n2)
  putStrLn $ "n2 <= n3: " ++ show (n2 <= n3)

  putStrLn $ "n1 > n2: " ++ show (n1 > n2)
  putStrLn $ "n2 > n3: " ++ show (n2 > n3)

  putStrLn $ "n1 >= n2: " ++ show (n1 >= n2)
  putStrLn $ "n2 >= n3: " ++ show (n2 >= n3)

  putStrLn "\nTesting Functions:"
  putStrLn "-----------------------"

  putStrLn $ "pred n1 = " ++ show (pred n1)
  putStrLn $ "fact n2 = " ++ show (fact n2)
  putStrLn $ "fib n3 = " ++ show (fib n3)

  putStrLn "\nTesting Operations:"
  putStrLn "-----------------------"

  let m1 = S (S (S (S O)))
      m2 = S (S (S O))

  putStrLn $ "min n1 m1 = " ++ show (min n1 m1)
  putStrLn $ "max n2 m2 = " ++ show (max n2 m2)

  putStrLn "\nTesting Division:"
  putStrLn "-----------------------"

  putStrLn $ "div n1 m2 = " ++ show (div n1 m2)
  putStrLn $ "div n1 O = " ++ show (div n1 O)

  putStrLn $ "quot n1 m2 = " ++ show (quot n1 m2)
  putStrLn $ "quot n1 O = " ++ show (quot n1 O)

  putStrLn "\nTesting Remainder:"
  putStrLn "-----------------------"

  putStrLn $ "rem n1 m2 = " ++ show (rem n1 m2)
  putStrLn $ "rem n1 O = " ++ show (rem n1 O)

  putStrLn "\nTesting GCD and LCM:"
  putStrLn "-----------------------"

  putStrLn $ "gcd n1 m2 = " ++ show (gcd n1 m2)
  putStrLn $ "gcd n1 O = " ++ show (gcd n1 O)

  putStrLn $ "lcm n1 m2 = " ++ show (lcm n1 m2)
  putStrLn $ "lcm n1 O = " ++ show (lcm n1 O)

  putStrLn "\nTesting Even and Odd:"
  putStrLn "-----------------------"

  putStrLn $ "ev n1 = " ++ show (ev n1)
  putStrLn $ "od n2 = " ++ show (od n2)

  putStrLn "\nAll tests completed."