data Note = A | B | C | D | E | F | G deriving (Eq,Show)
data Octave = One | Two | Three deriving Eq
data Pitch = Pitch Note Octave deriving Eq 

-- toPitch
isNote :: Char ->Bool
isNote char = case char of  
	'A' -> True
	'B' -> True
	otherwise -> False
isOctave :: Char ->Bool 
isOctave num = case num of
	'1' -> True
	'2' -> True
	'3' -> True
	otherwise -> False
toPitch :: String -> Maybe Pitch
toPitch x:y:[] 	
	| (isNote x) == True && (isOctave y) == True = Pitch 
  
toPitch _ = Nothing
