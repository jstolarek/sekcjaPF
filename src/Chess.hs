-- Ćwiczenie polega na implementacji gry w szachy dla dwóch graczy.  Cele
-- ćwiczenia:
--
--   * ogólne poćwiczenie programowania w Haskellu
--   * praktyczne wykorzystanie list comprehensions
--   * motywujący przykład do użycia monady stanu
--   * pretekst do zapoznania się z biblioteką haskeline
module Chess where

import Data.List
import Control.Monad

-- Reprezentacja bierek
data Piece = King
           | Queen
           | Bishop
           | Knight
           | Rook
           | Pawn

-- Kolory graczy
data Colour = White | Black

-- Wyświetlanie bierek i kolorów będzie nam potrzebne do stworzenia prostego
-- interfejsu użytkownika
instance Show Piece where
    show King   = "K"
    show Queen  = "Q"
    show Bishop = "B"
    show Knight = "N"
    show Rook   = "R"
    show Pawn   = "P"

instance Show Colour where
    show White = "W"
    show Black = "B"

-- Pozycja bierki na planszy jako współrzędne (rząd, kolumna)
type Position = (Int, Int)

-- Reprezentacja gracza w postaci listy jego bierek wraz z przypisanymi
-- aktualnymi pozycjami na planszy.
type Player = [(Piece, Position)]

-- Aktualny stan gry reprezentowany jako krotka zawierający listę bierek obu
-- graczy.  W implementacji założyłem, że pierwszy element krotki to gracz
-- wykonujący aktualny ruch, a drugi element krotki to jego przeciwnik.  Wynika
-- z tego, że po wykonaniu prawidłowego ruchu należy zamienić komponenty krotki
-- miejscami.
type GameState = (Player, Player)

-- Funkcja przyjmuje gracza i pozycję na planszy.  Jeśli na podanej pozycji
-- znajduje się figura gracza jest ona zwracana.  W przeciwnym wypadku zwracamy
-- Nothing.
getFigure :: Player -> Position -> Maybe Piece
getFigure = undefined

-- Prosta funkcja odpowiadająca na pytanie czy podana pozycja znajduje się na
-- planszy.  Założenie jest takie, że rzędy i kolumny o indeksie od 1 do 8
-- znajdują się na planszy.  Każda inna pozycja jest poza planszą.
isWithinBoard :: Position -> Bool
isWithinBoard = undefined

-- Funkjca przyjmuje typ bierki oraz jej pozycję i na podstawie tego zwraca
-- listę pozycji na którą figura może się przemieścić.  Ponieważ pionki
-- poruszają się tylko do przodu musimy również znać kolor figury wykonującej
-- ruch - bez tego nie wiemy w którą stronę może poruszyć się figura.  Jak widać
-- funkcja nie uwzględnia położenia na planszy figur przeciwnika.  Założenie
-- jest takie, że odpowiedzialna będzie za to funkjca makeMove.  Niestety, w tej
-- postaci funkcja nie pozwala na prawidłowe zaimplementowanie niektórych
-- ruchów.  Najbardziej dotkliwe jest niemożliwe wykonanie bicia pionem.  Wynika
-- to z tego, że pion może poruszyć się na skos tylko jeśli dokonuje w ten
-- sposób bicia.  Nie da się tego zrobić bez znajomości położenia figura
-- przeciwnika, a więc ta funkcja ewidentnie potrzebuje dostęou do GameState.
-- Ale nawet to nie wystarczy!  Piony mogą wykonywać bicie w przelocie.  Żeby je
-- zaimplementować potrzebna nam jest znajomość ostatniego ruchu.  Innym
-- dozwolonym ruchem w szachach jest roszada.  Do jej implementacji musimy
-- pamiętać, czy król oraz poszczególne wieże wykonały ruch od początku partii -
-- roszada jest dozwolona tylko wtedy kiedy żadna z bierek biorących w niej
-- udział nie wykonała wcześniej żadnego ruchu.  Tak więc przechowywana przez
-- nas informacja o stanie gry jest zdecydowanie niewystarczająca.
mayMoveTo :: Piece -> Position -> Colour -> [Position]
mayMoveTo King (r, c) _
    -- do utworzenia listy dozwolonych pozycji wykorzystamy list comprehensions
    = [ pos
      | x <- [-1, 0, 1]
      , y <- [-1, 0, 1]
      , let pos = (r + x, c + y)
      , isWithinBoard pos -- eliminacja pozycji poza planszą
      , (r, c) /= pos ]   -- nie umieszczamy pozycji bierki na liście wynikowej
mayMoveTo Queen _ _ = undefined -- ruch hetmana najlepiej zdefiniować jako
                                -- połączenie ruchu wieży i gońca
mayMoveTo Bishop _ _ = undefined
mayMoveTo Knight _ _ = undefined
mayMoveTo Rook   _ _ = undefined
mayMoveTo Pawn   _ _ = undefined -- ruch pionem wymaga znajomości jego koloru.
                                 -- Ponadto, pion na startowej pozycji może się
                                 -- ruszyć o dwa pola.

-- Funkcja mayMoveTo nie uwzględnia położenia bierek przciwnika (ani własnych)
-- przy wyznaczaniu dozwolonego ruchu.  Oznacza to, że musimy mieć jakiś sposób
-- aby zapobiec przeskakiwaniu bierek w momencie kiedy jest to niedozwolone.  W
-- tym celu użyjemy funkjci between.  Przyjmuje ona informację o rodzaju bierki
-- wykonującej ruch, polu z którego chcemy wykonać ruch i polu na które chcemy
-- wykonać ruch.  Wynikiem funkcji jest lista pozycji leżących w jednej linii
-- pomiędzy tymi polami z pominięciem pola startowego i końcowego.  Funkcja
-- będzie miała dwa przypadki.  Pierwszy przypadek będzie przypadkiem szczgólnym
-- związanym z ruchem konia.  Koń może przeskakiwać figury na planszy.  (Inna
-- sprawa, że dla konia pozycje startowa i końcowa nie spełniają założenia
-- znajdowania się na jedenj linii.)  Drugi przypadek będzie dotyczył każdej
-- innej figury.  Implementacja funkcji makeMove (poniżej) musi zagwarantować,
-- że funkcja between będzie wywołana tylko wtedy, kiedy pole startowe i
-- docelowe faktycznie leżą na jednej linii.
between :: Piece -> Position -> Position -> [Position]
between = undefined

-- Funkcja przyjmuje aktualny stan gry, kolor gracza wykonującego ruch, pozycję
-- z której ten gracz chce wykonać ruch oraz pozycję na którą chce wykonać ruch.
-- Jeśli możliwe jest wykonanie dozwolonego ruchu zwracany jest nowy stan gry.
-- Warunki wykonania prawidłowego ruchu:
--
--   * na polu z którego wykonywany jest ruch znajduje się bierka aktywnego
--     gracza
--
--   * na polu na które wykonywany jest ruch nie znajduje się bierka aktywnego
--     gracza
--
--   * pole na które wykonywany jest ruch znajduje się na liście pól na które
--     można wykonać dozwolony ruch (zwracane przez funkcję mayMoveTo)
--
--   * pomiędzy polem z którego wykonywany jest ruch i na polem na które
--     wykonywany jest ruch nie znajduje się żadna bierka zasłaniająca ruch
--     (funkcja between).
--
-- Należy zwrócić uwagę na konieczność usunięcia (zbicia) figury z pola na które
-- wykonywany jest ruch.  Oczywiście zablokowaliśmy możliwość poruszenia się na
-- pole na którym stoi bierka aktywnego gracza, co oznacza że zbijana bierka
-- musi należeć do przeciwnika.
makeMove :: GameState -> Colour -> Position -> Position -> Maybe GameState
makeMove = undefined

-- Usuwa z planszy figurę na podanej pozycji.  Jeśli nie ma tam żadnej figury
-- nic się nie dzieje.
removePiece :: Player -> Position -> Player
removePiece = undefined

-- Przyjmuje aktualny stan gry oraz aktywnego gracza.  Zwraca stan gry w którym
-- zamieniono graczy miejscami oraz zmieniono kolor aktywnego gracza.
switchPlayer :: GameState -> Colour -> (GameState, Colour)
switchPlayer = undefined


-- Wyświetlanie planszy
renderChessBoard :: GameState -> Colour -> String
renderChessBoard (first , second) colour = unlines
    [ unwords row
    | r <- [1..8]
    , let row = [ txt
                | c <- [1..8]
                , let txt | Just p <- getFigure white (r,c)
                          = show White ++ show p
                          | Just p <- getFigure black (r,c)
                          = show Black ++ show p
                          | otherwise
                          = show r ++ show c
                ]
    ]
    where (white, black) = case colour of
                             White -> (first , second)
                             Black -> (second, first )

-- Początkowe ustawienie bierek na planszy
initGame :: GameState
initGame = (
           [ ( Pawn   , (7, 1) )
           , ( Pawn   , (7, 2) )
           , ( Pawn   , (7, 3) )
           , ( Pawn   , (7, 4) )
           , ( Pawn   , (7, 5) )
           , ( Pawn   , (7, 6) )
           , ( Pawn   , (7, 7) )
           , ( Pawn   , (7, 8) )
           , ( Rook   , (8, 1) )
           , ( Knight , (8, 2) )
           , ( Bishop , (8, 3) )
           , ( Queen  , (8, 4) )
           , ( King   , (8, 5) )
           , ( Bishop , (8, 6) )
           , ( Knight , (8, 7) )
           , ( Rook   , (8, 8) )
           ] ,
           [ ( Pawn   , (2, 1) )
           , ( Pawn   , (2, 2) )
           , ( Pawn   , (2, 3) )
           , ( Pawn   , (2, 4) )
           , ( Pawn   , (2, 5) )
           , ( Pawn   , (2, 6) )
           , ( Pawn   , (2, 7) )
           , ( Pawn   , (2, 8) )
           , ( Rook   , (1, 1) )
           , ( Knight , (1, 2) )
           , ( Bishop , (1, 3) )
           , ( King   , (1, 4) )
           , ( Queen  , (1, 5) )
           , ( Bishop , (1, 6) )
           , ( Knight , (1, 7) )
           , ( Rook   , (1, 8) )
           ] )

-- Pętla główna gry.  Niestety, w żaden sposób nie przewiduje ona możliwości jej
-- zakończenia!  Nie ma również jakiejkolwiek obsługi błędów parsowania ani
-- nawet obsługi klawisz backspace.  Są to oczywiste mankamenty, które należy
-- poprawić aby gra miała jakiekolwiek pozory przyjazności dla użytkownika.  W
-- celu zapewnienia lepszej obsługi wczytywania linii poleceń warto
-- zainteresować się biblioteką haskeline.
runGame :: GameState -> Colour -> IO ()
runGame game player = do
  putStrLn (renderChessBoard game player)
  putStr ("Player " ++ show player ++ " move from: ")
  from <- read `liftM` getLine
  putStr ("Player " ++ show player ++ " move to: ")
  to <- read `liftM` getLine
  case makeMove game player from to of
    Nothing -> runGame game player
    Just game' -> let (game'', player') = switchPlayer game' player
                  in runGame game'' player'

-- W celu uruchomienia gry należy wpisać w ghci:
--
--  > runGame initGame White

-- Co dalej?  Po pierwsze, przydałaby się implementacja matowania.  Nie jest to
-- takie proste.  W sytuacji kiedy gracz jest zaszachowany trzeba przeanalizować
-- wszystkie możliwe ruchy wszystkimi figurami, aby sprawdzić czy którykolwiek z
-- nich pozwala na uniknięcie szacha.  Jeśli nie, oznacza to mat i gra się
-- kończy.  Ponadto, jeśli król jest zaszachowany, ale nie jest zamatowany,
-- należy dopuścić tylko te ruchy, które prowadzą do uniknięcia szachowania.
-- Jeszcze ciekawszą sytuacją jest wykrywanie patowania, a więc sytuacji kiedy
-- gracz zmuszony jest wykonać ruch, ale nie może go wykonać.

-- Po zaimplementowaniu wszystkich rodzajów ruchu nasz program będzie wyglądał
-- fatalnie.  Stan gry zostanie rozszerzony o całe mnóstwo dodatkowych
-- informacji, takich jak ostatni ruch albo informacja czy wieże i król się
-- poruszyły.  Te informacje będą potrzebne tylko w szczególnych sytuacjach
-- (bicie w przelocie, roszada).  Niemniej jednak będziemy musieli je
-- przekazywać wszędzie w sposób jawny.  Wyjściem z sytuacji jest zastosowanie
-- monad, a konkretnie monady stanu.  Będzie ona przechowywała stan gry, który
-- będzie można odczytywać i aktualizować wedle potrzeby.  Jawne przekazywanie
-- stanu gry nie będzie koniczne, co znacznie skróci i uprości kod.
