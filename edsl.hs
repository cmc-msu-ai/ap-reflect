{-# LANGUAGE TypeOperators #-}

import Debug.SimpleReflect hiding (reduce)
import Reflect


-- внутреннее представление операции (+)
(.+) :: (Num a, Show a) => a ~> a ~> a
(.+) = makeBinOp "+" (+)

main :: IO ()
main = do

  mapM_ print . reductions $ Val (.+) -$- Val (Just 1)
  putStrLn "------------------------------------------"

  mapM_ print . reductions $ Val (.+) -$- Val [1, 2, 3]
  putStrLn "------------------------------------------"

  mapM_ print . reductions $ Val (.+) -$- Val (Just 1) -*- Val (Just 3)
  putStrLn "------------------------------------------"

  mapM_ print . reductions $ Val (.+) -$- Val [a, b] -*- Val [x, y]
  putStrLn "------------------------------------------"
