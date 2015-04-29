{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
module SmartConditionals where

data N = Z | S N

data Vec a (n :: N) where
    Nil  :: Vec a 'Z
    Cons :: a -> Vec a n -> Vec a ('S n)

vecNull :: Vec a n -> Bool
vecNull Nil        = True
vecNull (Cons _ _) = False

vecTail :: Vec a ('S n) -> Vec a n
vecTail (Cons _ tl) = tl

{-
shorten :: Vec a n -> Vec a m
shorten xs = case vecNull xs of
               True  -> xs
               False -> vecTail xs
-}

data IsNull (n :: N) where
    Null    :: IsNull 'Z
    NotNull :: IsNull ('S n)

vecNull' :: Vec a n -> IsNull n
vecNull' Nil        = Null
vecNull' (Cons _ _) = NotNull

type family Pred (n :: N) :: N where
    Pred 'Z     = 'Z
    Pred ('S n) = n

shorten :: Vec a n -> Vec a (Pred n)
shorten xs = case vecNull' xs of
               Null    -> xs
               NotNull -> vecTail xs
