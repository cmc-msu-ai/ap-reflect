{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DeriveFunctor #-}

module Debug.Reflect where

import Control.Applicative (Applicative(..), (<$>))
import Data.List (findIndex, isInfixOf, isPrefixOf)
import Data.Char (isAlpha)


-- | analog of @(->)@
infixr 5 ~>
data (~>) a b = Fn String (a -> b) -- ^ name and function itself
  deriving (Functor)

-- | gets function
fromFn :: (a ~> b) -> (a -> b)
fromFn (Fn _ f) = f

instance Show (a ~> b) where
  show (Fn s _) = s

-- | adds brackets
parens :: String -> String
parens s = "(" ++ s ++ ")"

-- | checks whether function is infix
isInfixFn :: String -> Bool
isInfixFn (x:_) = not $ isAlpha x
isInfixFn [] = False

-- | shows function with its argument
showFn :: String -> String -> String
showFn f x = if isInfixFn f then x ++ " " ++ f else f ++ " " ++ x

-- | translates function @(a -> b -> c)@ into @(a ~> b ~> c)@
makeFn2 :: Show a => String -> (a -> b -> c) -> (a ~> b ~> c)
makeFn2 s f = Fn s $ \x -> Fn (showFn s (show x)) (f x)

-- | makes binary operation
makeBinOp :: Show a => String -> (a -> b -> c) -> (a ~> b ~> c)
makeBinOp s f = Fn s' $ \x -> Fn (parens $ showFn s (show x)) (f x)
  where s' = if isInfixFn s then parens s else s

-- | reflected expression
data Ap b
  = Val b
  | forall a. Show a => Ap (a ~> b) :$ Ap a

-- | checks whether expression is @Val@
isVal :: Ap a -> Bool
isVal (Val _) = True
isVal _ = False

-- | balances brackets
balanceParens :: String -> String
balanceParens s = if "<*>" `isInfixOf` s then parens s else s

-- | balances brackets
parensFr :: String -> String
parensFr s = if ' ' `elem` s then parens s else s

-- | shows operation application
showOp :: String -> String -> String -> String
showOp op f x
    | isInfixFn op = f ++ " " ++ op ++ " " ++ balanceParens x
    | otherwise = op ++ " " ++ f ++ " " ++ parensFr x

-- | shows operation application
showF :: String -> String -> String
showF f x
    | "fmap" `isPrefixOf` f = f ++ " " ++ parensFr x
    | otherwise = f ++ " " ++ balanceParens x

instance Show a => Show (Ap a) where
  show (Val x) = show x
  show (op :$ f :$ x) = showOp (show op) (show f) (show x)
  show (f :$ x) = showF (show f) (show x)

-- | analog of @fmap@ using @(~>)@
fmap' :: Functor f => (a ~> b) ~> f a ~> f b
fmap' = makeFn2 "<$>" (fmap . fromFn)

-- | analog of @pure@ using @(~>)@
pure' :: Applicative f => a ~> f a
pure' = Fn "pure" pure

-- | analog of @\<*>@ using @(~>)@
ap' :: (Show (f (a ~> b)), Applicative f) => f (a ~> b) ~> f a ~> f b
ap' = Fn "<*>" $ \f -> Fn (show f ++ " <*>") $ \x -> fromFn <$> f <*> x

-- | analog of  @\<$>@
infix 6 -$-
(-$-) :: (Show (f a), Functor f) => (a ~> b) -> f a -> Ap (f b)
f -$- x = Val fmap' :$ Val f :$ Val x

-- | analog of @\<*>@
infixl 5 -*-
(-*-) :: (Show (f (a ~> b)), Show (f a), Applicative f) => Ap (f (a ~> b)) -> f a -> Ap (f b)
f -*- x = Val ap' :$ f :$ Val x

-- | analog of @pure@
pure'' :: (Show a, Applicative f) => a -> Ap (f a)
pure'' x = Val pure' :$ Val x

-- | analog of @fmap@
fmap'' :: (Show (f a), Functor f) => (a ~> b) -> f a -> Ap (f b)
fmap'' f x = Val (makeFn2 "fmap" (fmap . fromFn)) :$ Val f :$ Val x

-- | reduces an expression
reduce'' :: Ap a -> Ap a
reduce'' (Val (Fn _ f) :$ Val x) = Val (f x)
reduce'' (Val f :$ x) = Val f :$ reduce'' x
reduce'' (f :$ x) = reduce'' f :$ x
reduce'' v = v

-- | reduces (evaluates) an expression once
reduce' :: Show a => Ap a -> Ap a
reduce' x
    | isVal x = x
    | otherwise = head $ filter (\x' -> show x' /= show x) xs
    where xs = iterate reduce'' x

-- | gets all reduction steps when evaluating an expression
reductions :: Show a => Ap a -> [Ap a]
reductions x = take (n + 1) xs
  where
    Just n = findIndex isVal xs
    xs = iterate reduce' x
