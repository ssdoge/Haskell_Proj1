data Note = A | B | C | D | E | F | G deriving (Eq,Show)
-- data Octave = One | Two | Three deriving Eq
-- data Pitch = Pitch Note Octave deriving Eq 

-- toPitch
toNote :: Char ->Maybe Note
toNote char = case char of  
	'A' -> Just A
	'B' -> Just B
	otherwise -> Nothing
toOctave :: Char ->Maybe Octave
toOctave num = case num of
	'1' -> Just One
	'2' -> Just Two
	'3' -> Just Three
	otherwise -> Nothing
toPitch :: String -> Maybe Pitch
toPitch x:y:[] 
toPitch _ = Nothing
