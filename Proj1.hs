module Proj1 (Pitch, toPitch, feedback, GameState, initialGuess, nextGuess) where
import Data.Maybe
import Data.Tuple
import Data.List

------------------Definition of data-------------------------------------------

data Note = A | B | C | D | E | F | G deriving (Eq,Ord,Show,Read)
data Octave = One | Two | Three deriving (Eq,Ord,Read,Show)
data Pitch = Pitch Note Octave deriving (Eq,Ord)
data GameState = GameState {candidate::[[Pitch]]}

instance Show Pitch where show = showPitch

--get Note&Octave from Pitch
getNote :: Pitch -> Note
getNote (Pitch n _ ) = n 

getOctave :: Pitch -> Octave
getOctave (Pitch _ o ) = o

------------------Definition of candidate Target-------------------------------

--generate the list of candidate pitches
genPitches :: [Note] -> [Octave] -> [Pitch]
genPitches [] _ = []
genPitches (x:xs) y = (map ((\ a b -> (Pitch a b)) x) y) ++ genPitches xs y

listOfPitch = genPitches [A,B,C,D,E,F,G] [One, Two, Three]

--generate combinations for Pitches 
combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations n xs = [ xs !! i : x | i <- [0..(length xs)-1],
						 x <- combinations (n-1) (drop (i+1) xs) ]

listOfTarget = combinations 3 listOfPitch

------------------Display------------------------------------------------------

--define the tables for changing datatpye
tableOfOctave = [(One,'1'),(Two,'2'),(Three,'3')]
tableOfNote = [(A,'A'),(B,'B'),(C,'C'),(D,'D'),(E,'E'),(F,'F'),(G,'G')]

--functions for changing datatype from Octave to Char, vice versa
fromOctave :: Octave -> Char
fromOctave x = fromJust (lookup x tableOfOctave)

toOctave :: Char -> Octave
toOctave x = fromJust (lookup x (map swap tableOfOctave))

--show function for Pitch
showPitch :: Pitch -> String
showPitch (Pitch n o) =	((head.show) n):(fromOctave o):[] 

--change datatpye from String to Pitch
--arguement x represents for Note, y represents for Octave
toPitch :: String -> Maybe Pitch
toPitch (x:y:[])
	| isJust (lookup x (map swap tableOfNote)) 
		&& isJust (lookup y (map swap tableOfOctave)) = Just pitch
 	| otherwise = toPitch "Nothing"
  	where pitch = Pitch (read [x]::Note) (toOctave y)
toPitch _ = Nothing

------------------Feedback-----------------------------------------------------

--single filter
sift :: [Pitch] -> Pitch -> [Pitch]
sift [] _ = []
sift (x:xs) y 
	| x == y = xs
	| otherwise = x:(sift xs y)  

--filter Target in order to count Note and Octave easily
--function returns unmatched Pitches in Target
filterTarget :: [Pitch] -> [Pitch] -> [Pitch]
filterTarget [] _ = []
filterTarget target guess = foldl sift target guess

--filter Guess to count Note and Octave easily
--function returns unmatched Pitches in Guess
filterGuess :: [Pitch] -> [Pitch] -> [Pitch]
filterGuess _ [] = []
filterGuess target guess = foldl sift guess target

--pitchMatch: count matched Pitch for entire guess 
pitchMatch :: [Pitch] -> [Pitch] -> Int
pitchMatch target guess = maxMatchNum - length (filterGuess target guess)
	where maxMatchNum = 3

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

--feedback function
feedback :: [Pitch] -> [Pitch] -> (Int, Int, Int)
feedback target guess = 
	(pitchMatch target guess, noteMatch target guess, octaveMatch target guess)  

------------------Initial Guess------------------------------------------------
--the intuition behind ["A1","B2","C3"] is quite easy. For Note, there are 
--seven cases which are all the same for us since we haven't got any infomation,
--and we make three different guesses in order to get more infomation from 
--feedback. Same for Octave.

initialGuess :: ([Pitch], GameState)
initialGuess = (map fromJust (map toPitch ["A1","B2","C3"]), 
					(GameState {candidate=listOfTarget}) )

------------------Next Guess---------------------------------------------------
--basic idea lies in hints. keep a candidate list, a.k.a GameState, and pare 
--the impossible chords down based on feedback from last guess. Then compute
--each chord in candidate list for possible remaining candidate numbers. Choose 
--the smallest one as next guess 

--pare down the impossible candidate 
--1st arg is candidate list, 2rd arg is last guess, 3rd arg is feedback
pare :: [[Pitch]] -> [Pitch] -> (Int, Int, Int) -> [[Pitch]]
pare list x y = filter (\a -> feedback x a == y) list
 
--a "loop" for finding then counting a certain feedback for a guess/chord
--1st arg is a list recording a certain feedback and corresponding repeated 
--number. 2rd arg is a feedback we want to join into the list 
find_Count :: [((Int,Int,Int), Int)]->(Int, Int, Int)->[((Int,Int,Int), Int)]
find_Count [] x = [(x, 1)]
find_Count ((x, num):xs) y
	| x == y = (x, num+1):xs 
	| otherwise = (x, num):find_Count xs y

--for a chord in candidate list, count all different possible feedbacks and 
--corresponding number of occurrences. 
--1st arg is candidate list, 2rd arg is the chord, 3rd arg is previous 
--statistics. notice that it is a "loop"
chordCount :: [[Pitch]] -> [Pitch] -> [((Int,Int,Int), Int)] 
												-> [((Int,Int,Int), Int)]
chordCount [] _ table = table
chordCount (x:xs) y table = let k = (feedback x y) in
	chordCount xs y (find_Count table k)

--combine above two functions to give an evaluation for a certain chord
expectedRemainNum :: [[Pitch]] -> [Pitch] -> Float
expectedRemainNum list x =
	sum (map (\(_, n) -> fromIntegral (n*n) / fromIntegral len) k)
	where 
		k = chordCount list x []  
		len = length k

--pick a chord which is most likely to leave a smallest remaining candidate list
--1st arg is for iteration or "loop". 2rd arg is the whole candidate list which
--keeps unchanged in iteration. 3rd arg is current minimum and 4th arg is 
--corresponding chord which is current best guess.
pickOne :: [[Pitch]] -> [[Pitch]] -> Float -> [Pitch] -> [Pitch]
pickOne [] _ min best = best
pickOne (x:xs) list min curBest 
	| k < min = pickOne xs list k x
	| otherwise = pickOne xs list min curBest
	where k = expectedRemainNum list x

--an integration for above all 
nextGuess :: ([Pitch],GameState) -> (Int,Int,Int) -> ([Pitch],GameState)
nextGuess (pitches, gs) y =
	 (pickOne list list init_min emptyList , GameState {candidate=list})
	where 
		list = pare (candidate gs) pitches y
		init_min = 1330
		emptyList = []


