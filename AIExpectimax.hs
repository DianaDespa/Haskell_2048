{- Despa Diana Alexandra 321CA -}

module AIExpectimax where

import Board
import Interactive
import Data.List
import Data.Maybe
import Control.Monad (liftM2)

{-
    Tipul 'Tree a' este definit cu ajutorul unui constructor ce primeşte ca
	argumente o valoarea din rădăcină și o listă de subarbori. 
-}
data Tree a = MakeTree a [(Tree a)]

{-
	Întoarce valoarea din rădăcina unui arbore.
-}
root :: Tree a -> a
root (MakeTree x list) = x

{-
	Întoarce lista de subarbori descendenți ai unui arbore.
-}
desc :: Tree a -> [Tree a]
desc (MakeTree x list) = list

{-
	Întoarce o listă de mutări valide, care produc o schimbare in configurația
	tablei.
-}
moveFunctions :: Board -> [Board]
moveFunctions b = valid
	where
	all = [moveLeft b, moveRight b, moveUp b, moveDown b]
	valid = [x | x <- all, x /= b]

{-
	Întoarce o listă cu toate configurațiile posibile ale tablei ce pot apărea
	la următoarea generare aleataore a unei noi celule. Pentru fiecare celulă
	liberă,	sunt generate două configurații, având în acea celulă valorile 2
	și respectiv 4.
-}
genRandomFunction :: Board -> [Board]
genRandomFunction board = concat $ map (choice board) availablePosIndices
	where
	availablePosIndices = elemIndices 0 (concat (rows board))
	choice :: Board -> Int -> [Board]
	choice board index = [build (setInMatrix (rows board) 2 i j) (score board),
					build (setInMatrix (rows board) 4 i j) (score board)]
					where
					i = index `div` 4
					j = index `rem` 4

{-
    Întoarce tabla rezultată din aplicarea unei mutări alese de o euristică
    în contextul expectimax.
-}
move :: Board -> Board
move board = pick staticFunction $ prune levels tree
	where
	genFunctions = intersperse genRandomFunction $ repeat moveFunctions
	tree = expand genFunctions board
	levels = 3
	staticFunction :: Board -> Float
	staticFunction board = fromIntegral $ score board
	
{-
    Construiește un arbore expectimax (eventual infinit), pornind de la lista
    funcțiilor de generare per nivel și de la rădăcină.

    Pentru generarea succesorilor nivelului k, se utilizează funcția
    de pe poziția k din listă. Pentru arbori infiniți, această listă va fi
    de asemenea infinită.
-}
expand :: [a -> [a]] -> a -> Tree a
expand fctList r = MakeTree r descendants
	where
	descendants
		| null fctList = []
		| otherwise = map (expand (tail fctList)) ((head fctList) r)
				
{-
	Limitează un arbore la numărul de niveluri dat ca parametru.
-}
prune :: Int -> Tree a -> Tree a
prune level tree
	| level == 0 = MakeTree (root tree) []
	| otherwise = MakeTree (root tree) $ map (prune (level - 1)) $ desc tree

{-
    Determină valoarea expectimax a unui nod MAX. Funcția de evaluare statică
    este dată ca parametru.
-}
maximize :: (a -> Float) -> Tree a -> Float
maximize fct tree
	| null (desc tree) = fct $ root tree
	| otherwise = maximum $ map (expect fct) $ desc tree

{-
    Determină valoarea expectimax a unui nod ȘANSĂ. Funcția de evaluare statică
    este dată ca parametru.
-}
expect :: (a -> Float) -> Tree a -> Float
expect fct tree
	| null (desc tree) = fct $ root tree
	| otherwise = average $ map (maximize fct) $ desc tree

{-
    Întoarce cheia copilului rădăcinii arborelui expectimax, ales
    în conformitate cu principiul algoritmului. Funcția de evaluare statică
    este dată ca parametru.
-}
pick :: (a -> Float) -> Tree a -> a
pick fct tree = root $ fromJust $ lookup max descList
	where
	descVals = map (expect fct) $ desc tree
	descList = zip descVals $ desc tree
	max = maximum descVals

{-
    Calculează media unei liste.
-}
average :: [Float] -> Float
average = liftM2 (/) sum genericLength

{-
    Urmărește pas cu pas evoluția jocului, conform strategiei implementate.
-}
userMode :: IO ()
userMode = ai move