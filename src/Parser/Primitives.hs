module Parser.Primitives (
  Parser(..),

  char, word,
  number, literal, token,
  iter, while, cons,

  (<|>), (<=>), (>>>), (?>>>), (>>>=),
  (<+>), (<-+>), (<+->), (<?+>), (<+?>)
 ) where

import Control.Monad
import Data.Char


-- Typ danych (tak naprawdę synonim typu), którego użyjemy do reprezentacji
-- parserów.  Parser do parsowania wartości typu a to funkcja która przyjmuje
-- jako argument String do sparsowania.  W wyniku parsowania zwracana jest
-- wartość typu a oraz pozostały String, którego parser wartości typu a nie był
-- w stanie sparsować.  Ponieważ parsowanie może się nie udać, dlatego całość
-- opakowana jest w typ Maybe. Jeśli parsowanie się powiedzie zwracany wynik
-- jest opakowany w Just.  Jeśli parsowanie się nie uda zwracane jest Nothing.
type Parser a = String -> Maybe (String, a)


-- Ćwiczenie polega na uzupełnieniu implementacji funkcji podanych poniżej.
-- Starałem się ułożyć funkcje w kolejności rosnącej trudności. To oznacza, że
-- po skończeniu zadania w pliku będzie nieco bałaganu.  Należy go wsyprzątać
-- grupując funkcje tematycznie.  W wielu przypadkach nie podaję parametrów
-- funkcji, co oznacza że trzeba je dopisać samodzielnie.  W większości
-- przypadków jest to oczywiste, ale jest też kilka podchwytliwych przykładów.


-- Parser do parsowania pojedynczego znaku.  Jedyną sytuacją w której takie
-- parsowanie może się nie powieść jest przekazanie do parsowania pustego ciągu
-- znaków.
char :: Parser Char
char []     = Nothing
char (c:cs) = Just (cs, c)


-- Aby móc konstruować bardziej złożone parsery potrzebne nam będą funkcje
-- łączące proste funkcje w bardziej złożone.  Takie funkcje nazywa się
-- "kombinatorami".  Aby nasz kod był zwięzły, ale jednocześnie łatwy do
-- zrozumienia nadamy naszym kombinatorom postać operatorów wraz z przypisaniem
-- odpowiednich priorytetów


-- Nasz pierwszy kombinator łączy parser m z predykatem p.  Jeśli parser m
-- sparsuje z powodzeniem wartość typu a, to parsowanie się powiedzie jeśli
-- predykat p będzie prawdziwy dla sparsowanej wartości.
infix 7 <=>
(<=>) :: Parser a -> (a -> Bool) -> Parser a
(m <=> p) cs =
  case m cs of
    Nothing -> Nothing
    Just (cs', a) -> if p a
                     then return (cs', a)
                     else Nothing


-- Uwaga: wynik zwracany przez parser zamknięty jest w typie Maybe.  Typ Maybe
-- jest monadą, co daje nam możliwość zastosowania notacji do:
--
-- (<=>) :: Parser a -> (a -> Bool) -> Parser a
-- (m <=> p) cs = do
--    (cs', a) <- m cs
--    if p a then return (cs', a) else Nothing
--
-- Postaraj się zaimplementować kombinatory najpierw używając notacji zwykłej, a
-- potem przepisując go na notację do.  Nie zawsze jest to możliwe, ale żadne z
-- rozwiązań w tym pliku nie wymaga stosowania deklaracji let albo where.
-- Kombinatory zajmują najczęściej 3 linijki w notacji do, natomiast zwykłe
-- funkcje zawsze są jednolinijkowe.


-- Parser pojedynczej litery uzyskujemy poprzez połączenie parsowania
-- pojedynczego znaku z funkcją sprawdzającą, czy sparsowany znak jest
-- literą.
letter :: Parser Char
letter = char <=> isLetter


-- Parsowanie spacji
space :: Parser Char
space = undefined


-- Parser zwracający ustalony rezultat.  Przykład: `result 123` utworzy parser,
-- który zawsze zwróci 123 jako wynik parsowania.  String przekazany do
-- parsowania pozostaje bez zmian.
result :: a -> Parser a
result = undefined


-- Parser, który zawsze kończy się niepowodzeniem.
failure :: Parser a
failure = undefined


-- Napiszmy teraz kilka kombinatorów.  Pozwolą nam one na skonstruowanie
-- bardziej złożonych parserów.


-- Odpowiednik logicznego "lub" dla parserów: najpierw podejmowana jest próba
-- parsowania przy użyciu parsera m.  Jeśli się powiedzie, rezultat jest
-- zwracany.  Jeśli się nie powiedzie, to podejmowana jest próba parsowania przy
-- użyciu parsera n.
infixl 3 <|>
(<|>) :: Parser a -> Parser a -> Parser a
(m <|> n) cs = undefined


-- Odpowiednik logicznego "i" dla parserów: najpierw wykonywane jest parsowanie
-- parserem m, potem pozostałość stringa jest parsowana parserem n.  Jeśli oba
-- parsowania się powiodły, wyniki są pakowane w krotkę i zwracane.
infixl 6 <+>
(<+>) :: Parser a -> Parser b -> Parser (a, b)
(m <+> n) cs = undefined


-- Działanie tego kombinatora wynika bezpośrednio z jego sygnatury typu.
infixl 5 >>>
(>>>) :: Parser a -> (a -> b) -> Parser b
(m >>> f) cs = undefined


-- Zdefiniujemy sobie jeszcze funkcję pomocniczną, do przerabiania krotek na
-- listy.  Taka potrzeba wynika bezpośrednio z konstrukcji kombinatora <+>,
-- który opakowuje wynik w krotkę.
cons :: (a, [a]) -> [a]
cons (hd, tl) = hd : tl


-- Funkcja iteruje dany parser do momentu aż parsowanie się nie powiedzie.  Jest
-- to najbardziej złożona funkcja do tej pory: wymaga zastosowania trzech
-- kombinatorów zdefiniowanych powyżej oraz funkcji result.
iter :: Parser a -> Parser [a]
iter = undefined


-- Mając zdefiniowaną funkcję iter możemy teraz łatwo napisać parser do
-- parsowania ciągów znaków spełniających podany predykat
while :: (Char -> Bool) -> Parser String
while = undefined


-- Funkcja while posłuży nam teraz do zdefiniowana parsera słów, a więc ciągów
-- znaków nie zawierających w sobie spacji.
word :: Parser String
word = undefined


-- Zajmiemy się teraz parsowaniem liczb.  Będzie to zadanie nieco
-- trudniejsze. Zacznijmy od napisania parsera pojedynczych cyfr.  Pomocna
-- będzie funkcja digitToInt oraz wcześniej zdefiniowane kombinatory.
digit :: Parser Int
digit = undefined


-- Oczywiście parsowanie pojedynczych cyfr nie wystarczy - pojedyncze cyfry
-- trzeba złożyć w liczbę.  W tym celu wykorzystamy poniższą funkcję pomocniczą:
buildNumber :: Int -> Int -> Int
buildNumber a b = a * 10 + b


-- Potrzebny nam będzie jeszcze poniższy kombinator:
infixl 4 >>>=
(>>>=) :: Parser a -> (a -> Parser b) -> Parser b
(m >>>= n) cs = undefined


-- Wyposażeni w >>>= i buildNumber możemy napisać parser liczb.  Będzie on
-- składał się z dwóch funkcji.  Funkcja number sparsuje pierwszą cyfrę i
-- przekaże parsowanie do funkcji number'
number :: Parser Int
number = undefined


-- Funkcja number' otrzymuje liczbę sparsowaną do tej pory i podejmuje próbę
-- sparsowania kolejnej cyfry.  Jeśli próba się powiedzie tworzona jest nowa
-- liczba i parsowanie jest kontynuowane rekurencyjnie.  Jeśli próba sparsowania
-- kolejnej cyfry się nie powiedzie, zwracamy jako wynik dotychczas sparsowaną
-- liczbę.
number' :: Int -> Parser Int
number' = undefined


-- Pora zająć się parsowaniem podanych wyrazów.  Na początek stworzymy parser
-- akceptujący tylko podany znak.  Przykład: `literal 'a'` pomyślnie sparsuje
-- każdy ciąg znaków zaczynający się od "a".  Dla każdego innego ciągu znaków
-- parsowanie się nie powiedzie.
literal :: Char -> Parser Char
literal = undefined


-- Mając możliwość parsowania jednego znaku możemy zająć się parsowaniem całych
-- wyrazów.  Funkcja token stworzy parser, który będzie akceptował podany ciąg
-- znaków.
token :: String -> Parser String
token []     = undefined
token (c:cs) = undefined


-- Funkcja iter' jest wariantem funkcji iter.  W przypadku funkcji iter możliwe
-- było sparsowanie zerowej ilości wyrażeń, tzn. nie powodowało to błędu
-- parsowania - po prostu zwracaliśmy pusta listę.  Funkcja iter' wymaga, aby
-- udało się sparsować przynajmniej jedno wyrażenie typu a.
iter' :: Parser a -> Parser [a]
iter' m = m <+> iter m >>> cons <|> failure


-- Ćwiczenie zakończymy tworząc jeszcze kilka bardzo użytecznych kombinatorów i
-- jedną funkcję.


infixl 5 ?>>>
(?>>>) :: Parser a -> (a -> Maybe b) -> Parser b
(m ?>>> f) cs = undefined


-- Kombinator <-+> dokonuje parsowania obydwoma przekazanymi parserami m i n.
-- Wynik pierwszego parsera jest odrzucany, wynik drugiego parsera jest
-- zwracany.  Jeśli przynajmniej jedno parsowanie zakończy się porażką, wynik
-- kombinatora też kończy się porażką.
infixl 6 <-+>
(<-+>) :: Parser a -> Parser b -> Parser b
(m <-+> n) cs = undefined


-- Kombinator <+-> dokonuje parsowania obydwoma przekazanymi parserami m i n.
-- Wynik drugiego parsera jest odrzucany, wynik pierwszego parsera jest
-- zwracany.  Jeśli przynajmniej jedno parsowanie zakończy się porażką, wynik
-- kombinatora też kończy się porażką.
infixl 6 <+->
(<+->) :: Parser a -> Parser b -> Parser a
(m <+-> n) cs = undefined


-- Analogicznie do <-+>, ale dopuszczamy sytuację, w której parsowanie pierwszym
-- parserem zakończy się niepowodzeniem.
infixl 6 <?+>
(<?+>) :: Parser a -> Parser b -> Parser b
(m <?+> n) cs = undefined


-- Analogicznie do <+->, ale dopuszczamy sytuację, w której parsowanie drugim
-- parserem zakończy się niepowodzeniem.
infixl 6 <+?>
(<+?>) :: Parser a -> Parser b -> Parser a
(m <+?> n) cs = undefined


-- Na koniec dobrze jest wysprzątać plik poprzez pogrupowanie funkcji w bloki
-- tematyczne.  Kombinatory najlepiej umieścić na końcu pliku, porządkując je w
-- kolejności wiązania (od najniższej do najwyższej).
