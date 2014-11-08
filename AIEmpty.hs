{- Despa Diana Alexandra 321CA -}

module AIEmpty where

import Board
import Interactive
import Data.Function
import Data.List

{-
	Întoarce numărul de celule libere de pe tablă.
-}
freeCells :: Board -> Int
freeCells board = foldr zeroCount 0 $ concat $ rows board
	where
	zeroCount = \x count -> if (x == 0) then (count + 1) else count :: Int

{-
    Întoarce tabla rezultată din aplicarea acelei mutări care maximizează
    numărul de celule libere.
-}
move :: Board -> Board
move board = last sortedMoves
	where
		moves = [moveLeft board, moveRight board, moveUp board, moveDown board]
		valid = [x | x <- moves, x /= board]
		freeCellsNr = map freeCells valid
		sortedMoves = map fst $ sortBy (compare `on` snd) (zip valid freeCellsNr)

{-
    Urmărește pas cu pas evoluția jocului, conform strategiei implementate.
-}
userMode :: IO ()
userMode = ai move