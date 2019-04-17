--Author: Junrong Su
--login Id: Junrongs
--The purpose of this file is to implement both the guessing
--and answering parts of a logical guessing game called 'The Game of Musician'

--this code is separated into four parts, as definition of data, display for
--data, calculate feedback for two chords and guess a chord.

module Proj1 (Pitch, toPitch, feedback,
                    GameState, initialGuess, nextGuess) where
import Data.Maybe
import Data.Tuple (swap)
import Data.List
import Data.Map (insertWith, toList, empty, elems, Map)

------------------Definition of data-------------------------------------------
--a pitch contains note and octave, and a game state store the possible target

data Note = A | B | C | D | E | F | G deriving (Eq,Ord,Show,Read)
data Octave = One | Two | Three deriving (Eq,Ord)
data Pitch = Pitch Note Octave deriving (Eq,Ord)
data GameState = GameState {candidate::[[Pitch]]}

instance Show Pitch where show = showPitch

------------------Definition of candidate Target-------------------------------

--a list of all legal pitches, used to generate a chord
listOfPitch = [(Pitch x y) | x <- [A,B,C,D,E,F,G], y <- [One, Two, Three]]

--generate combinations from listOfPitches
combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations n xs = [ xs !! i : x | i <- [0..(length xs)-1],
                         x <- combinations (n-1) (drop (i+1) xs) ]

listOfTarget = combinations 3 listOfPitch   --a chord contains 3 pitches

------------------Display------------------------------------------------------
--contains show pitch method and transform a string to pitch method

--define the table for changing datatpye from Octave to Char, vice versa
tableOfOctave = [('1', One), ('2', Two), ('3', Three)]

--show function for Pitch, use pairs for changing data
showPitch :: Pitch -> String
showPitch (Pitch n o) = show n ++[fromJust $ lookup o $ map swap tableOfOctave]

--change datatpye from String to Pitch
--arguement x represents for Note, y represents for Octave
--if input contains 2 qualified characters, return corresponding pitch,
--else return Nothing
toPitch :: String -> Maybe Pitch
toPitch (x:y:[])
    | elem x tableOfNote && elem y octaveList = Just pitch
    | otherwise = toPitch "Nothing"
    where
        pitch = Pitch (read [x]::Note) (fromJust $ lookup y tableOfOctave)
        tableOfNote = "ABCDEFG"
        octaveList = ['1','2','3']
toPitch _ = Nothing

------------------Feedback-----------------------------------------------------

--using '\\'' to exclude same elem in the list, since a chord has length of 3,
--the parameter in function is fixed 3. By deduct the length of complement,
--we get the number of common elements.
elemCount :: (Ord a) => [a] -> [a] -> Int
elemCount x y = lenOfInput - length (x \\ y)
    where lenOfInput = 3

--change a list of Pitches into a Note list and an Octave list
toNOList :: [Pitch] -> ([Note],[Octave])
toNOList [] = ([],[])
toNOList ((Pitch n o):xs) = (n:noteList, o:octaveList)
    where
        noteList = fst (toNOList xs)
        octaveList = snd (toNOList xs)

--feedback function
--since indentical pitches will be counted repeatedly when counting octaves
--and notes, we should deduct them.
--the param names represent their meaning
feedback :: [Pitch] -> [Pitch] -> (Int, Int, Int)
feedback target guess =
    (numOfPitch, numOfNote - numOfPitch, numOfOctave - numOfPitch)
    where
        numOfPitch = elemCount guess target
        numOfNote = elemCount nt ng
        numOfOctave = elemCount ot og
        (nt,ot) = toNOList target
        (ng,og) = toNOList guess

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
--each chord in candidate list for possible remaining candidate numbers.
--Choose the smallest one as next guess

--pare down the impossible candidate
--1st arg is candidate list, 2rd arg is last guess, 3rd arg is feedback
pare :: [[Pitch]] -> [Pitch] -> (Int, Int, Int) -> [[Pitch]]
pare candidate x fb = filter (\a -> feedback x a == fb) candidate

--for a chord in candidate list, count all different possible feedbacks and
--corresponding number of occurrences.
--1st arg is candidate list, 2rd arg is the chord, 3rd arg is previous
--statistics. notice that it is a "loop"
--To count the occurence of different feedback pattern, use a hashMap to speed
--up
chordCount :: [[Pitch]] -> [Pitch] -> Map (Int, Int, Int) Int
                                                -> Map (Int, Int, Int) Int
chordCount [] _ map = map
chordCount (x:xs) y map = let k = (feedback x y) in
    chordCount xs y (insertWith (\a b -> b+1) k 1 map)

--combine above two functions to give an evaluation for a certain chord
--1st arg is candidate list, 2rd arg is a certain chord, ouput is the sum of
--n^2/len
expectedRemainNum :: [[Pitch]] -> [Pitch] -> Float
expectedRemainNum list x =
    sum (map (\n -> fromIntegral (n*n) / fromIntegral len) k)
    where
        k = elems (chordCount list x empty)
        len = length k

--pick a chord which is most likely to leave a smallest remaining candidate
--list. 1st arg is a candidate chord. 2rd arg is the whole candidate list which
--keeps unchanged in iteration. 3rd arg is current minimum and 4th arg is
--corresponding chord which is current best guess. Output is the best choice
pickOne :: [[Pitch]] -> [[Pitch]] -> Float -> [Pitch] -> [Pitch]
pickOne [] _ min best = best
pickOne (x:xs) list min curBest
    | k < min = pickOne xs list k x
    | otherwise = pickOne xs list min curBest
    where k = expectedRemainNum list x

--an integration for above all, there are 1330 candidate, so initial min should
-- >= 1330
nextGuess :: ([Pitch],GameState) -> (Int,Int,Int) -> ([Pitch],GameState)
nextGuess (pitches, gs) y =
     (pickOne list list init_min curBest , GameState {candidate=list})
    where
        list = pare (candidate gs) pitches y
        init_min = 1330
        curBest = []
