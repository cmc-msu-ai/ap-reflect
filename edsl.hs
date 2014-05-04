{-# LANGUAGE TypeOperators #-}

import Debug.SimpleReflect
import Reflect


-- внутреннее представление операции (+)
(.+) :: (Num a, Show a) => a ~> a ~> a
(.+) = makeBinOp "+" (+)

main :: IO ()
main = do

  mapM_ print . reductions $ (.+) -$- Just 1
  putStrLn "------------------------------------------"

  mapM_ print . reductions $ (.+) -$- [1, 2, 3]
  putStrLn "------------------------------------------"

  mapM_ print . reductions $ (.+) -$- Just 1 -*- Just 3
  putStrLn "------------------------------------------"

  mapM_ print . reductions $ (.+) -$- [a, b] -*- [x, y]
  putStrLn "------------------------------------------"
