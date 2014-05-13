--  позволяет определять операторы на уровне типов (~>)
{-# LANGUAGE TypeOperators #-}
-- позволяет определить Ap (а именно :$ )
{-# LANGUAGE ExistentialQuantification #-}
-- позволяет писать deriving (Functor), что автоматически выводит instance Functor
{-# LANGUAGE DeriveFunctor #-}

module Debug.Reflect where

import Control.Applicative (Applicative(..), (<$>))
import Data.List (findIndex, isInfixOf, isPrefixOf)
import Data.Char (isAlpha)


-- тип данных ~> - внутреннее представление функции (a -> b)
-- т.е. ее символьное обозначение и сама функция
-- правая ассоциативность оператора ~>, приоритет 5
infixr 5 ~>
data (~>) a b = Fn String (a -> b) deriving (Functor)

-- получение функции
fromFn :: (a ~> b) -> (a -> b)
fromFn (Fn _ f) = f

-- отображение функции
instance Show (a ~> b) where
  show (Fn s _) = s

-- добавление скобок
parens :: String -> String
parens s = "(" ++ s ++ ")"

-- является ли запись функции инфиксной
isInfixFn :: String -> Bool
isInfixFn (x:_) = not $ isAlpha x

-- отображение функции в зависимости от синтаксиса
showFn :: String -> String -> String
showFn f x = if isInfixFn f then x ++ " " ++ f else f ++ " " ++ x

-- перевод функции (a -> b -> c) во внутреннее представление (a ~> b ~> c)
makeFn2 :: Show a => String -> (a -> b -> c) -> (a ~> b ~> c)
makeFn2 s f = Fn s $ \x -> Fn (showFn s (show x)) (f x)

-- перевод бинарной операции во внутреннее представление
makeBinOp :: Show a => String -> (a -> b -> c) -> (a ~> b ~> c)
makeBinOp s f = Fn s' $ \x -> Fn (parens $ showFn s (show x)) (f x)
  where s' = if isInfixFn s then parens s else s

-- структура выполняемых выражений
data Ap b
  = Val b
  | forall a. Show a => Ap (a ~> b) :$ Ap a

-- является ли выражение значением
isVal :: Ap a -> Bool
isVal (Val _) = True
isVal _ = False

-- баланс скобок
balanceParens :: String -> String
balanceParens s = if "<*>" `isInfixOf` s then parens s else s

parensFr :: String -> String
parensFr s = if ' ' `elem` s then parens s else s

-- отображение применения операций
showOp :: String -> String -> String -> String
showOp op f x
    | isInfixFn op = f ++ " " ++ op ++ " " ++ balanceParens x
    | otherwise = op ++ " " ++ f ++ " " ++ parensFr x

showF :: String -> String -> String
showF f x
    | "fmap" `isPrefixOf` f = f ++ " " ++ parensFr x
    | otherwise = f ++ " " ++ balanceParens x

-- отображение выражения
instance Show a => Show (Ap a) where
  show (Val x) = show x
  show (op :$ f :$ x) = showOp (show op) (show f) (show x)
  show (f :$ x) = showF (show f) (show x)

-- аналог fmap
fmap' :: Functor f => (a ~> b) ~> f a ~> f b
fmap' = makeFn2 "<$>" (fmap . fromFn)

-- аналог pure
pure' :: Applicative f => a ~> f a
pure' = Fn "pure" pure

-- аналог ap
ap' :: (Show (f (a ~> b)), Applicative f) => f (a ~> b) ~> f a ~> f b
ap' = Fn "<*>" $ \f -> Fn (show f ++ " <*>") $ \x -> fromFn <$> f <*> x

-- внутреннее представление операции <$>
infix 6 -$-
(-$-) :: (Show (f a), Functor f) => (a ~> b) -> f a -> Ap (f b)
f -$- x = Val fmap' :$ Val f :$ Val x

-- внутреннее представление операции <*>
infixl 5 -*-
(-*-) :: (Show (f (a ~> b)), Show (f a), Applicative f) => Ap (f (a ~> b)) -> f a -> Ap (f b)
f -*- x = Val ap' :$ f :$ Val x

-- внутреннее представление операции pure
pure'' :: (Show a, Applicative f) => a -> Ap (f a)
pure'' x = Val pure' :$ Val x

-- внутреннее представление операции fmap
fmap'' :: (Show (f a), Functor f) => (a ~> b) -> f a -> Ap (f b)
fmap'' f x = Val (makeFn2 "fmap" (fmap . fromFn)) :$ Val f :$ Val x

-- сокращение выражения
reduce'' :: Ap a -> Ap a
reduce'' (Val (Fn _ f) :$ Val x) = Val (f x)
reduce'' (Val f :$ x) = Val f :$ reduce'' x
reduce'' (f :$ x) = reduce'' f :$ x
reduce'' v = v

-- один шаг сокращения (вычисления) выражения
reduce' :: Show a => Ap a -> Ap a
reduce' x
    | isVal x = x
    | otherwise = head $ filter (\x' -> show x' /= show x) xs
    where xs = iterate reduce'' x

-- все шаги сокращения (вычисления) выражения
reductions :: Show a => Ap a -> [Ap a]
reductions x = take (n + 1) xs
  where
    Just n = findIndex isVal xs
    xs = iterate reduce' x
