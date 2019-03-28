import Data.Maybe
import Data.Tuple
import Data.List

data Note = A | B | C | D | E | F | G deriving (Eq,Ord,Show,Read)
data Octave = One | Two | Three deriving (Eq,Ord,Read,Show)
data Pitch = Pitch Note Octave deriving (Eq,Ord)

instance Show Pitch where show = showPitch

testPitch1 = [(Pitch A Two), (Pitch F One) , (Pitch D Three) ]
testPitch2 = [(Pitch C One), (Pitch A One) , (Pitch B Three) ]
testPitch3 = [(Pitch C Two), (Pitch C One) , (Pitch B Three) ]

-- Define the tables for changing datatpye
tableOfoctave = [(One,'1'),(Two,'2'),(Three,'3')]
tableOfNote = [(A,'A'),(B,'B'),(C,'C'),(D,'D'),(E,'E'),(F,'F'),(G,'G')]

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

--feedBack function: x:xs for target y:ys for guess

{--singlePitchMatch: first arg is target , the second is single guess
singlePitchMatch :: [Pitch] -> Pitch -> Int
singlePitchMatch [] _ = 0
singlePitchMatch ((Pitch n1 o1):xs) (Pitch n2 o2)
	| n1 == n2 && o1 == o2 = 1
	| otherwise = singlePitchMatch xs (Pitch n2 o2)
-}

--pitchMatch: count matched Pitch for entire guess 
pitchMatch :: [Pitch] -> [Pitch] -> Int
pitchMatch _ [] = 0
pitchMatch target (x:xs) =
	length (filter (x==) target) + pitchMatch target xs

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



--noteMatch:
singleNoteOctaveMatch :: Pitch -> Pitch -> (Bool,Bool)
singleNoteOctaveMatch (Pitch n1 o1) (Pitch n2 o2) 
	| n1 == n2 && o1 /= o2 = (True, False)
	| n1 /= n2 && o1 == o2 = (False, True)
	| otherwise = (False, False)

noteMatch :: [Pitch] -> [Pitch] -> Int
noteMatch _ [] = 0
noteMatch target (x:xs) = 
	length (filter ( (True,False)== ) (map (singleNoteOctaveMatch x) target)) + 
		noteMatch target xs
octaveMatch :: [Pitch] -> [Pitch] -> Int
octaveMatch _ [] = 0
octaveMatch target (x:xs) =
	length (filter ( (False,True)== ) (map (singleNoteOctaveMatch x) target)) +
		octaveMatch target xs
{-
feedBack :: [Pitch] -> [Pitch] -> (Int, Int, Int)
feedBack x y = (pitchMatch x y,  )  
-}











