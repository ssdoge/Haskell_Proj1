data Note = A | B | C | D | E | F | G deriving (Eq,Show,Read)
data Octave = One | Two | Three deriving (Eq,Show,Read)
data Pitch = Pitch (Maybe Note) (Maybe Octave) deriving (Eq)
instance Show Pitch where show = showPitch
--show
showNote :: Note -> Maybe Char
showNote x  
	| x == A = Just 'A'
	| x == B = Just 'B'
	| x == C = Just 'C'
	| x == D = Just 'D'
	| x == E = Just 'E'
	| x == F = Just 'F'
	| x == G = Just 'G'
	| otherwise = Nothing
showOctave :: Octave -> Maybe Char
showOctave x 
	| x == One = Just '1'
	| x == Two = Just '2'
	| x == Three = Just '3'
	| otherwise = Nothing

showPitch :: Pitch ->  String
showPitch (Pitch n o) =	 (showNote n):(showOctave o):[] 
showPitch (Pitch _ _ ) = Nothing

-- toPitch
isNote :: Char ->Bool
isNote c 
	| c == 'A' || c == 'B' || c == 'C' || c == 'D' 
		|| c == 'E' || c == 'F' || c == 'G' = True
	| otherwise = False

isOctave :: Char ->Bool 
isOctave num = case num of
	'1' -> True
	'2' -> True
	'3' -> True
	otherwise -> False
transNote :: Char -> Maybe Note
transNote x 
	| x == 'A' = Just A
	| x == 'B' = Just B
	| x == 'C' = Just C
	| x == 'D' = Just D
	| x == 'E' = Just E
	| x == 'F' = Just F
	| x == 'G' = Just G
	| otherwise = Nothing

transOctave:: Char -> Maybe Octave
transOctave x 
	| x == '1' = Just One
	| x == '2' = Just Two
	| x == '3' = Just Three
	| otherwise = Nothing

toPitch :: String -> Maybe Pitch
toPitch (x:y:[])
	| isNote x == True && isOctave y == True = Just pitch
  	| otherwise = toPitch []
  	where pitch = Pitch (transNote x) (transOctave y)
toPitch _ = Nothing


