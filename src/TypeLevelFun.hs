{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}

module TypeLevelFun where

-- GADTs = Generalized Algebraic Data Types

-- data () = ()  -- unit

{-

           *                       'Bool       kinds

--------------------------------------------

  Int              Bool         'True 'False   types
                                      |
--------------------------------------+-----
                                      |
 3 5 7           True False           |        values
                                      |
                                      |
   po promocji za pomocą rozszerzenia |
   DataKinds Bool staje się kindem, --+
   a True i False typami

-}

-- CZĘŚĆ 1: konstrukcja funkcji zwracającej wartości różnych typów w zależności
-- od wartości parametru wejściowego.

data Bool where
    False :: Bool
    True  :: Bool

-- singleton bool
data SBool (a :: Bool) where
    SFalse :: SBool False
    STrue  :: SBool True

data Nat where
    Zero :: Nat
    Succ :: Nat -> Nat

-- parametr `a` nazywamy indeksem ponieważ jego wartość różni się w zależności
-- od konstruktora
data SNat (a :: Nat) where
    SZero :: SNat Zero
    SSucc :: SNat a -> SNat (Succ a)

-- Wartości typu SNat idą w parze z typem:
--
--   SZero               :: SNat Zero
--   SSucc SZero         :: SNat (Succ Zero)
--   SSucc (SSucc SZero) :: SNat (Succ (Succ Zero))
--
-- Jest to cecha typów singletonowych: znając wartość znamy typ i na odwrót.
-- Dlatego typy singletonowe łączą ze sobą to co się dzieje na poziomie wartości
-- z tym co się dzieje na poziomie typów.

-- tutaj z kolei konstruktory nie nakładają żadnych ograniczeń na wartość
-- parametru `a`, w związku z czym nazywamy go po prostu parametrem typu
-- (ang. type paremeter)
data List a where
    Nil  ::                List a -- Nil  = []
    Cons :: a -> List a -> List a -- Cons = :

type family F (a :: Bool) :: * where
    F True  = Nat
    F False = ()

foo :: SBool a -> F a
foo STrue  = Succ Zero -- a = True , foo :: SBool True  -> Nat
foo SFalse = ()        -- a = False, foo :: SBool False -> ()

-- typy funkcyjne, w których zwracany typ zależy od wartości wejścia nazywane są
-- pi-types. Jest to jedna z cech systemów typów znanych jako typy zależne
-- (ang. dependent types). Przykładowe języki z typami zależnymi to Idris i
-- Agda. Języki te nie wymagają korzystania z singletonów.


-- CZĘŚĆ 2: zweryfikowane porównywanie liczb naturalnych

(>=) :: Nat -> Nat -> Bool
_        >=  Zero    = True
Zero     >= _        = False
(Succ a) >= (Succ b) = a >= b

-- Succ (Succ Zero) >= Succ Zero <==> Succ Zero >= Zero

-- Typ GEq reprezentuje relację >= pomiędzy dwoma liczbami naturalnymi.
-- Ograniczenia nałożone na indeksy konstruktorów GEq powodują, że możliwe jest
-- stworzenie wartości typu GEq dla tych indeksów, dla których relacja >= jest
-- prawdziwa.
data GEq (a :: Nat) (b :: Nat) where
    GZero ::            GEq a Zero
    GSucc :: GEq a b -> GEq (Succ a) (Succ b)

-- Typ Order reprezentuje uporządkowanie dwóch liczb naturalnych.  Każdy
-- konstruktor przechowuje dowód, że dane dwie liczby istotnie są w relacji >=
-- lub <=
data Order (a :: Nat) (b :: Nat) where
    Ge :: GEq a b -> Order a b
    Le :: GEq b a -> Order a b

-- Funkcja order zwraca informację o uporządkowaniu dwóch liczb wraz z dowodem
-- tego uporządkowania. Nie da się jej źle zaimplementować, bo każda pomyłka
-- skończy się błędem kompilacji.
order :: SNat a -> SNat b -> Order a b
order (SSucc _)  SZero    = Ge GZero
order  SZero    (SSucc _) = Le GZero
order (SSucc a) (SSucc b) = case order a b of
                              Ge ageb -> Ge (GSucc ageb)
                              Le bgea -> Le (GSucc bgea)

-- Dalsza lektura:
--
-- * "Designing Dependently-Typed Programming Languages" - seria czterech
-- wykładów Stephanie Weirich pokazująca jak od podstaw skonstruować prosty
-- język z typami zależnymi. Link:
--   https://www.cs.uoregon.edu/research/summerschool/summer14/curriculum.html
