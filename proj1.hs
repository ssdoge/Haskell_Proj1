--module Proj1 (Pitch, toPitch, feedback, GameState, initialGuess, nextGuess) where
import Data.Maybe
import Data.Tuple
import Data.List
------------------Definition----------------------------------------------
data Note = A | B | C | D | E | F | G deriving (Eq,Ord,Show,Read)
data Octave = One | Two | Three deriving (Eq,Ord,Read,Show)
data Pitch = Pitch Note Octave deriving (Eq,Ord)
data GameState = ()
instance Show Pitch where show = showPitch

testPitch1 = [(Pitch A Two), (Pitch F One) , (Pitch D Three) ]
testPitch2 = [(Pitch C One), (Pitch A One) , (Pitch B Three) ]
testPitch3 = [(Pitch C Two), (Pitch C One) , (Pitch B Three) ]
testP0 = map fromJust (map toPitch ["D1","B1","G2"])
testP1 = map fromJust (map toPitch ["A1","B1","C2"])
testP2 = map fromJust (map toPitch ["A1","D1","E2"])
testP3 = map fromJust (map toPitch ["A1","F1","G2"])
testP4 = map fromJust (map toPitch ["B1","D1","G2"])

testP00 = map fromJust (map toPitch ["A1","B2","A3"])
testP000 = map fromJust (map toPitch ["A1","A2","B1"])


-- Define the tables for changing datatpye
tableOfoctave = [(One,'1'),(Two,'2'),(Three,'3')]
tableOfNote = [(A,'A'),(B,'B'),(C,'C'),(D,'D'),(E,'E'),(F,'F'),(G,'G')]

------------------Display-------------------------------------------------
-- functions for changing datatype from Octave <-> Char
fromOctave :: Octave -> Char
fromOctave x = fromJust (lookup x tableOfoctave)

toOctave :: Char -> Octave
toOctave x = fromJust (lookup x (map swap tableOfoctave))

-- show function for Pitch
showPitch :: Pitch -> String
showPitch (Pitch n o) =	((head.show) n):(fromOctave o):[] 

-- change datatpye from String to Pitch
toPitch :: String -> Maybe Pitch
toPitch (x:y:[])
	| isJust (lookup x (map swap tableOfNote)) 
		&& isJust (lookup y (map swap tableOfoctave)) = Just pitch
 	| otherwise = toPitch []
  	where pitch = Pitch (read [x]::Note) (toOctave y)
toPitch _ = Nothing
------------------Feedback------------------------------------------------
--single filter
sift :: [Pitch] -> Pitch -> [Pitch]
sift [] _ = []
sift (x:xs) y 
	| x == y = xs
	| otherwise = x:(sift xs y)  

--filter Target in order to count Note and Octave easily
filterTarget :: [Pitch] -> [Pitch] -> [Pitch]
filterTarget [] _ = []
filterTarget target guess = foldl sift target guess

--filter Guess to count Note and Octave easily
filterGuess :: [Pitch] -> [Pitch] -> [Pitch]
filterGuess _ [] = []
filterGuess target guess = foldl sift guess target

--pitchMatch: count matched Pitch for entire guess 
pitchMatch :: [Pitch] -> [Pitch] -> Int
pitchMatch target guess = 3 - length (filterGuess target guess)

--two-Pointers method for counting 
twoPointersCountNote :: [Pitch] -> [Pitch] -> Int
twoPointersCountNote [] _ = 0
twoPointersCountNote _ [] = 0
twoPointersCountNote ((Pitch n1 o1):xs) ((Pitch n2 o2):ys) 
	| n1 == n2 = 1 + twoPointersCountNote xs ys
	| n1 < n2 = twoPointersCountNote xs ((Pitch n2 o2):ys)
	| otherwise = twoPointersCountNote ((Pitch n1 o1):xs) ys

twoPointersCountOctave :: [Pitch] -> [Pitch] -> Int
twoPointersCountOctave [] _ = 0
twoPointersCountOctave _ [] = 0
twoPointersCountOctave ((Pitch n1 o1):xs) ((Pitch n2 o2):ys) 
	| o1 == o2 = 1 + twoPointersCountOctave xs ys
	| o1 < o2 = twoPointersCountOctave xs ((Pitch n2 o2):ys)
	| otherwise = twoPointersCountOctave ((Pitch n1 o1):xs) ys
--noteMatch: using above method on list sorted by note order
noteMatch :: [Pitch] -> [Pitch] -> Int
noteMatch _ [] = 0
noteMatch target guess = twoPointersCountNote ft fg
	where 
		ft = sortBy (\(Pitch n1 _) (Pitch n2 _) 
				-> compare n1 n2) (filterTarget target guess)
		fg = sortBy (\(Pitch n1 _) (Pitch n2 _) 
				-> compare n1 n2) (filterGuess target guess)

--octaveMatch: using above method on list sorted by octave
octaveMatch :: [Pitch] -> [Pitch] -> Int
octaveMatch _ [] = 0
octaveMatch target guess = twoPointersCountOctave ft fg
	where 
		ft = sortBy (\(Pitch _ o1) (Pitch _ o2) 
				-> compare o1 o2) (filterTarget target guess)
		fg = sortBy (\(Pitch _ o1) (Pitch _ o2) 
				-> compare o1 o2) (filterGuess target guess)

--feedBack function
feedBack :: [Pitch] -> [Pitch] -> (Int, Int, Int)
feedBack target guess = 
	(pitchMatch target guess, noteMatch target guess,
		octaveMatch target guess)  
------------------initial-------------------------------------------------

initialGuess :: ([Pitch],GameState)
initialGuess = (map fromJust (map toPitch ["A1","B2","C3"]) , 
				())

nextGuess :: ([Pitch],GameState) → (Int,Int,Int) → ([Pitch],GameState)
nextGuess 





































