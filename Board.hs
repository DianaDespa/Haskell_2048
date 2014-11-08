{- Despa Diana Alexandra 321CA -}

{-
    Tabla de joc și mutările posibile.

    Modulul exportă numai funcțiile enumerate mai jos, ceea ce înseamnă
    că doar acestea vor fi vizibile din alte module. Justificarea vizează
    prevenirea accesului extern la structura obiectelor 'Board', și a eventualei
    coruperi a consistenței interne a acestora.
-}
module Board
    ( Board
    , build
    , rows
    , score
    , initialize
    , placeRandomCell
    , moveUp
    , moveDown
    , moveLeft
    , moveRight
    , isWon
    , isLost
	, setInMatrix
    ) where

import System.Random
import Data.List
import Network.CGI.Protocol

{-
    Tipul 'Board' este definit cu ajutorul unui constructor ce primeşte ca
	argumente o matrice (o listă de liste) de întregi şi un întreg. 
-}
data Board = MakeBoard [[Int]] Int deriving Eq

{-
	Se instanțiază clasa 'Show' cu tipul 'Board'.
-}
instance Show Board where
	show (MakeBoard matrix score) = concatMap (++"\n") (map showRow matrix) ++
									"Score: " ++ (show score) ++ "\n"
		where
		showRow = \list -> concat $ ["\t  "] ++ (intersperse "\t| " (replace "0" "." (map show list)))

{-
    Construiește o tablă de joc pe baza unei configurații, furnizate pe linii,
    și a unui scor.
-}
build :: [[Int]] -> Int -> Board
build = MakeBoard

{-
    Întoarce configurația tablei de joc.
-}
rows :: Board -> [[Int]]
rows (MakeBoard matrix score) = matrix 

{-
    Întoarce scorul curent al tablei de joc.
-}
score :: Board -> Int
score (MakeBoard matrix score) = score

{-
	"Înlocuieşte" o valoare dintr-o listă cu o valoare dată.
	Primeşte ca parametrii o listă, o valoare şi un indice(întreg).
	Întoarce o listă asemenea celei primite ca parametru, având pe poziţia
	indicată noua valoare.
-}
setInList :: [a] -> a -> Int -> [a]
setInList list val pos
	| pos < 0 = list
	| otherwise = case splitAt pos list of
				(front, x:xs) -> front ++ val:xs
				_ -> list

{-
	Asemănător cu funcţia setInList, această funcţie "înlocuieşte" valoarea
	dintr-o listă de liste, de la coordonatele primite ca parametrii, cu o nouă
	valoare.
-}
setInMatrix :: [[a]] -> a -> Int -> Int -> [[a]]
setInMatrix matrix val row col = setInList matrix (setInList ((!!) matrix row) val col) row

{-
    Plasează aleator o nouă celulă pe tabla de joc.
-}	
placeRandomCell :: RandomGen g => Board -> g -> (Board, g)
placeRandomCell board g = ((build newRows (score board)), newPosGen)
	where
	interval = (0, 1) :: (Int, Int)
	(coin, newNrGen) = randomR interval g
	newNr = if coin == 0 then 2 else 4
	availablePosIndices = elemIndices 0 (concat (rows board))
	(newPosIndex, newPosGen) = randomR (0, (length availablePosIndices) - 1) g
	pos = (!!) availablePosIndices newPosIndex
	rowNr = pos `div` 4
	colNr = pos `rem` 4
	newRows = setInMatrix (rows board) newNr rowNr colNr
		  
{-
    Generează aleator o tablă de joc cu două celule ocupate.
-}
initialize :: RandomGen g => g -> (Board, g)
initialize g = placeRandomCell firstNumBoard g1
	where
	zeroMatrix 	= [ [ 0, 0, 0, 0 ]
			, [ 0, 0, 0, 0 ]
			, [ 0, 0, 0, 0 ]
			, [ 0, 0, 0, 0 ]]
	(firstNumBoard, g1) = placeRandomCell (build zeroMatrix 0) g

{-
	Întoarce o listă în care toate zerourile din lista primită ca parametru
	sunt mutate la sfârşit.
-}
shiftZeros :: [Int] -> [Int]
shiftZeros list = nonZero ++ take (4 - length nonZero) (repeat 0)
	where nonZero = filter (/= 0) list

{-
	Întoarce o pereche formată dintr-o listă și un scor. Lista este obținută
	prin mutarea spre stânga a elementelor listei parametru, și "unirea" celor
	alăturate care au valori egale. Scorul reprezintă suma valorilor celulelor
	obținute în urma "unirilor".
-}
shiftLeft :: [Int] -> ([Int], Int)
shiftLeft (x : y : xs)
	| x == 0 = (x : y : xs, 0)
	| x == y = ((2 * x) : list1 ++ [0], (2 * x) + addScore1)
	| otherwise = (x : list2, addScore2)
		where
		(list1, addScore1) = shiftLeft xs
		(list2, addScore2) = shiftLeft (y : xs)
shiftLeft list = (list, 0)

{-
	Aplică funcția shiftLeft lista din matricea dată ca parametru, având un
	anumit indice(x).
-}
shiftHelper :: [[Int]] -> Int -> ([Int], Int)
shiftHelper matrix x = shiftLeft $ shiftZeros $ (!!) matrix x

{-
    Realizează mutarea la stânga.
-}
moveLeft :: Board -> Board
moveLeft board = build newRows newScore
	where
	pairList = map (shiftHelper (rows board)) [0..3]
	newRows = map fst pairList
	newScore = (score board) + (sum $ map snd pairList)
	
{-
	Rotește o matrice cu 90 de grade în sens antiorar.
-}
rotateCClk90 :: [[Int]] -> [[Int]]
rotateCClk90 = reverse . transpose

{-
	Rotește o matrice cu 90 de grade în sens orar.
-}
rotateClk90 :: [[Int]] -> [[Int]]
rotateClk90 = map reverse . transpose

{-
	Rotește o matrice cu 180 de grade.
-}
rotate180 :: [[Int]] -> [[Int]]
rotate180 = rotateCClk90 . rotateCClk90

{-
	Primeşte două funcții de rotire și un Board. Rotește Board-ul conform cu
	prima funcţie, apoi aplică mutarea la stânga, apoi îl rotește conform cu a
	doua funcţie.
-}
moveHelper :: ([[Int]] -> [[Int]]) -> ([[Int]] -> [[Int]]) -> Board -> Board
moveHelper op1 op2 board = build (op2 (rows makeMove)) (score makeMove)
	where
		makeMove = moveLeft(build (op1 (rows board)) (score board))

{-
    Realizează mutarea jos, rotind întâi la stânga și apoi la dreapta.
-}
moveUp :: Board -> Board
moveUp = moveHelper rotateCClk90 rotateClk90

{-
    Realizează mutarea la dreapta, rotind tabla cu 180 de grade de două ori.
-}
moveRight :: Board -> Board
moveRight = moveHelper rotate180 rotate180

{-
    Realizează mutarea sus, rotind întâi la dreapta și apoi la stânga. 
-}
moveDown :: Board -> Board
moveDown = moveHelper rotateClk90 rotateCClk90

{-
    Întoarce 'True' dacă tabla conține o configurație câștigătoare,
    i.e. există cel puțin o celulă cu 2048.
-}
isWon :: Board -> Bool
isWon board = elem 2048 $ concat $ rows board

{-
	Întoarce 'True' dacă există două elemente de aceeași valoare pe una dintre
	liniile unei matrici.
-}
orizontalAdjacent :: [[Int]] -> Bool
orizontalAdjacent matrix
	| null matrix = False
	| otherwise = test (head matrix) || orizontalAdjacent (tail matrix)
	where
	test x
		| length x == 1 = False
		| otherwise = (head x == head (tail x)) || (test (tail x))

{-
	Întoarce 'True' dacă există două elemente de aceeași valoare pe una dintre
	coloanele unei matrici (rotește matricea și verifică aceeași condiție pe
	linii). 
-}
verticalAdjacent :: [[Int]] -> Bool
verticalAdjacent = orizontalAdjacent . rotateClk90

{-
    Întoarce 'True' dacă tabla conține o configurație în care jucătorul pierde,
    i.e. nu există nicio celulă liberă, și nici nu există celule vecine egale,
    pe orizontală sau verticală.
-}
isLost :: Board -> Bool
isLost board = not $ (any (== 0) (concat (rows board)))
				|| (orizontalAdjacent (rows board))
				|| (verticalAdjacent (rows board))