module Main where

import qualified Nat as N

main :: IO ()
main = do
  putStrLn "Testing Nat module..."

  -- Test natural numbers
  let n1 = N.O
  let n2 = N.S N.O
  let n3 = N.S (N.S N.O)

  putStrLn "Natural Numbers:"
  putStrLn $ "n1: " ++ show n1
  putStrLn $ "n2: " ++ show n2
  putStrLn $ "n3: " ++ show n3

  -- Test operations
  putStrLn "\nOperations:"
  putStrLn $ "n1 + n2: " ++ show (n1 N.+ n2)
  putStrLn $ "n2 * n3: " ++ show (n2 N.* n3)
  putStrLn $ "n3 ^ n1: " ++ show (n3 N.^ n1)
  putStrLn $ "n3 - n1: " ++ show (n3 N.- n1)

  -- Test comparisons
  putStrLn "\nComparisons:"
  putStrLn $ "n1 == n2: " ++ show (n1 N.== n2)
  putStrLn $ "n2 /= n3: " ++ show (n2 N./= n3)
  putStrLn $ "n3 < n2: " ++ show (n3 N.< n2)
  putStrLn $ "n3 >= n2: " ++ show (n3 N.>= n2)

  -- Test list functions
  let list1 = N.Cons n1 (N.Cons n2 (N.Cons n3 N.Nil))

  putStrLn "\nList Functions:"
  putStrLn $ "List: " ++ show list1
  putStrLn $ "Length: " ++ show (N.length list1)
  putStrLn $ "Sum: " ++ show (N.sum list1)
  putStrLn $ "Product: " ++ show (N.product list1)
  putStrLn $ "Filter Even: " ++ show (N.filterEven list1)
  putStrLn $ "Filter Odd: " ++ show (N.filterOdd list1)
  putStrLn $ "Is Sorted: " ++ show (N.isSorted list1)

  -- Add more test cases as needed...

  putStrLn "\nTesting complete."