import Data.Maybe
import Data.Tuple

data Note = A | B | C | D | E | F | G deriving (Eq,Show,Read)
data Octave = One | Two | Three deriving (Eq,Read,Show)
data Pitch = Pitch Note Octave deriving (Eq)

instance Show Pitch where show = showPitch

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



