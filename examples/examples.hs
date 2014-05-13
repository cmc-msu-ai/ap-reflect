{-# LANGUAGE TypeOperators #-}

import Control.Applicative
import Control.Monad.Identity

import Debug.SimpleReflect
import Reflect

-- внутреннее представление операций

-- (+)
(.+) :: (Num a, Show a) => a ~> a ~> a
(.+) = makeBinOp "+" (+)

-- (-)
(.-) :: (Num a, Show a) => a ~> a ~> a
(.-) = makeBinOp "-" (-)

-- (*)
(.*) :: (Num a, Show a) => a ~> a ~> a
(.*) = makeBinOp "*" (*)

-- (/)
(./) :: (Fractional a, Show a) => a ~> a ~> a
(./) = makeBinOp "/" (/)

-- (++)
(.++) :: (Show a) => [a] ~> [a] ~> [a]
(.++) = makeBinOp "++" (++)

-- (:)
(.:) :: (Show a) => a ~> [a] ~> [a]
(.:) = makeBinOp ":" (:)

sequenceA :: (Show a, Show (f a), Show (f [a]), Show (f ([a] ~> [a])), Applicative f) => [f a] -> Ap (f [a])
sequenceA [] = pure'' []
sequenceA (x:xs) = Val ap' :$ ((.:) -$- x) :$ sequenceA xs

traverse :: (Show a, Show b, Show (f b), Show (f [b]), Show (f ([b] ~> [b])), Applicative f) => (a -> f b) -> [a] -> Ap (f [b])
traverse _ [] = pure'' []
traverse f (x:xs) = Val ap' :$ (Val fmap' :$ Val (.:) :$ fx) :$ traverse f xs
  where fx = Val (Fn "f" f) :$ Val x

instance Show a => Show (ZipList a) where
  show (ZipList x) = "ZipList " ++ show x

instance Show m => Show (Const m a) where
  show (Const x) = "Const " ++ addParens (show x)
    where addParens s = if ' ' `elem` s then parens s else s

instance Show a => Show (Identity a) where
  show (Identity x) = "Identity " ++ addParens (show x)
    where addParens s@(x:_) = if x /= '(' && ' ' `elem` s then parens s else s

main :: IO ()
main = do

  let line = putStrLn "------------------------------------------"

  let 
    g' = makeBinOp "g" (g :: Expr -> Expr -> Expr)
    h' = makeBinOp "h" (h :: Expr -> Expr -> Expr)
  mapM_ print . reductions $ Val [fromFn g' 1, fromFn h' 2] -*- [a, b, c]
  line

  mapM_ print . reductions $ makeBinOp "f" (f :: Expr -> Expr -> Expr) -$- Just a -*- Just b
  line
  mapM_ print . reductions $ Val (Just (fromFn (.+) a)) -*- Just b
  line
  mapM_ print . reductions $ Val (Just (Fn "(+ a)" (+ a))) -*- Just b
  line

  mapM_ print . reductions $ fmap'' (.+) (Just a)
  line
  mapM_ print . reductions $ fmap'' (.+) (Just a) -*- Just b
  line
  mapM_ print . reductions $ pure'' (.+) -*- Just a -*- Just b
  line

  mapM_ print . reductions $ (.+) -$- Identity a
  line
  mapM_ print . reductions $ (.+) -$- Identity a -*- Identity b
  line

  mapM_ print . reductions $ (.+) -$- Const (a + b)
  line
  mapM_ print . reductions $ (.+) -$- Const a -*- Const b
  line

  mapM_ print . reductions $ (.+) -$- ZipList [1,2,3] -*- ZipList [100,100,100]
  line
  mapM_ print . reductions $ makeBinOp "max" max -$- ZipList [1,2,3,4,5,3] -*- ZipList [5,3,1,2] 
  line
  mapM_ print . reductions $ Val (ZipList [fromFn (.+) 1, fromFn (.*) 100]) -*- ZipList [a,b]
  line

  let f x = if even (length x) then Just (head x) else Nothing
  mapM_ print . reductions $ traverse f ["ab","cdef","gh"]
  line
  mapM_ print . reductions $ sequenceA [Just a, Just b, Just c]
  line
  mapM_ print . reductions $ sequenceA [Just a, Nothing, Just c]
  line
  mapM_ print . reductions $ makeBinOp "f" (+) -$- [x, y]
  line

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
  