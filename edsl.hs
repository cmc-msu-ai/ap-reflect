{-# LANGUAGE TypeOperators #-}

import Debug.SimpleReflect
import Reflect

-- внутреннее представление операции (+)
(.+) :: (Num a, Show a) => a ~> a ~> a
(.+) = makeBinOp "+" (+)

-- внутреннее представление операции (-)
(.-) :: (Num a, Show a) => a ~> a ~> a
(.-) = makeBinOp "-" (-)

-- внутреннее представление операции (*)
(.*) :: (Num a, Show a) => a ~> a ~> a
(.*) = makeBinOp "*" (*)

-- внутреннее представление операции (/)
(./) :: (Fractional a, Show a) => a ~> a ~> a
(./) = makeBinOp "/" (/)

-- внутреннее представление операции (++)
(.++) :: (Show a) => [a] ~> [a] ~> [a]
(.++) = makeBinOp "++" (++)


main :: IO ()
main = do

  let line = putStrLn "------------------------------------------"

  mapM_ print . reductions $ (.++) -$- Just "hello " -*- Just "world"
  line
  mapM_ print . reductions $ (.+) -$- ("1 + ", 1) -*- ("6 =", 6)
  line
  mapM_ print . reductions $ (.*) -$- Just 3 -*- Just 2
  line
  mapM_ print . reductions $ (./) -$- Just 3 -*- Just 2
  line
  mapM_ print . reductions $ (.+) -$- Just 1
  line
  mapM_ print . reductions $ (.+) -$- Just 1 -*- Just 3
  line
  mapM_ print . reductions $ (.+) -$- Just a -*- Just b
  line
  mapM_ print . reductions $ (.-) -$- Just a -*- Just b
  line
  mapM_ print . reductions $ (.+) -$- Nothing
  line
  mapM_ print . reductions $ (.+) -$- Just a -*- Nothing
  line
  mapM_ print . reductions $ (.+) -$- Nothing -*- Just b
  line
  mapM_ print . reductions $ (.+) -$- [1, 2, 3]
  line
  mapM_ print . reductions $ (.+) -$- [a, b] -*- [x, y]
  line
  mapM_ print . reductions $ (.+) -$- []
  line
  mapM_ print . reductions $ (.+) -$- [1,2] -*- []
  line
  mapM_ print . reductions $ (.+) -$- Right a -*- Left b
  line
  mapM_ print . reductions $ (.+) -$- Left a -*- Right b
  line



  


