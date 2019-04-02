module Proj1 (Pitch, toPitch, feedback, GameState, initialGuess, nextGuess) where
import Data.Maybe
import Data.Tuple
import Data.List

------------------Definition of data-------------------------------------------

data Note = A | B | C | D | E | F | G deriving (Eq,Ord,Show,Read)
data Octave = One | Two | Three deriving (Eq,Ord)
data Pitch = Pitch Note Octave deriving (Eq,Ord)
data GameState = GameState {candidate::[[Pitch]]}

instance Show Pitch where show = showPitch

------------------Definition of candidate Target-------------------------------

--the list of candidate pitches
listOfPitch = [(Pitch x y) | x <- [A,B,C,D,E,F,G], y <- [One, Two, Three]]

--generate combinations for Pitches 
combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations n xs = [ xs !! i : x | i <- [0..(length xs)-1],
						 x <- combinations (n-1) (drop (i+1) xs) ]

listOfTarget = combinations 3 listOfPitch

------------------Display------------------------------------------------------

--define the table for changing datatpye
tableOfOctave = [('1', One), ('2', Two), ('3', Three)]

--show function for Pitch, use pairs for changing data
showPitch :: Pitch -> String
showPitch (Pitch n o) =	show n ++[fromJust $ lookup o $ map swap tableOfOctave]

--change datatpye from String to Pitch
--arguement x represents for Note, y represents for Octave
toPitch :: String -> Maybe Pitch
toPitch (x:y:[])
	| elem x tableOfNote && elem y octaveList
		= Just pitch
 	| otherwise = toPitch "Nothing"
  	where
  	 pitch = Pitch (read [x]::Note) (fromJust $ lookup y tableOfOctave)
  	 tableOfNote = "ABCDEFG"
  	 octaveList = ['1','2','3']
toPitch _ = Nothing

------------------Feedback-----------------------------------------------------

--count pitch using "elem", first arg is what we want to find, the second arg
--is a lookup table
pitchCount :: [Pitch] -> [Pitch] -> Int
pitchCount [] _ = 0 
pitchCount (x:xs) y 
	| elem x y == True = 1 + pitchCount xs y
	| otherwise = pitchCount xs y

--for a sorted [Pitch] using two pointers method 
noteCount :: [Pitch] -> [Pitch] -> Int
noteCount [] _ = 0
noteCount _ [] = 0
noteCount ((Pitch n1 o1):xs) ((Pitch n2 o2):ys) 
	| n1 == n2 = 1 + noteCount xs ys
	| n1 < n2 = noteCount xs ((Pitch n2 o2):ys) 
	| otherwise = noteCount ((Pitch n1 o1):xs) ys 

--
countOctave :: [Pitch] -> [Int] -> [Int]
countOctave [] x = x
countOctave ((Pitch _ o):xs) [a, b, c]
	| o == One = countOctave xs [a+1, b, c]
	| o == Two = countOctave xs [a, b+1, c]
	| o == Three = countOctave xs [a, b, c+1]

matchedOctave :: [Pitch] -> [Pitch] -> Int
matchedOctave x y = sum $ map (\(m, n) -> if m<n then m else n) $ zip a b
	where 
		a = countOctave x [0,0,0]
		b = countOctave y [0,0,0]

--feedback function
feedback :: [Pitch] -> [Pitch] -> (Int, Int, Int)
feedback target guess = (a, b-a, c-a)
	where 
		a = pitchCount target guess
		b = noteCount t g
		t = sort target
		g = sort guess
		c = matchedOctave target guess

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

