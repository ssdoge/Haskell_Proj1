data Note = A | B | C | D | E | F | G deriving (Eq,Show,Read)
data Octave = One | Two | Three deriving (Eq,Show,Read)
data Pitch = Pitch Note Octave deriving Eq 

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
transNote :: Char -> Note
transNote x 
	| x == 'A' = A
	| x == 'B' = B
	| x == 'C' = C
	| x == 'D' = D
	| x == 'E' = E
	| x == 'F' = F
	| x == 'G' = G


transOctave:: Char -> Octave
transOctave x 
	| x == '1' = One
	| x == '2' = Two
	| x == '3' = Three
	

toPitch :: String -> Maybe Pitch
toPitch (x:y:[])
	| isNote x == True && isOctave y == True = Just pitch
  	| otherwise = toPitch []
  	where pitch = Pitch (transNote x) (transOctave y)
  	
toPitch _ = Nothing
