module Hello where

-- A type to represent one of the six volleyball positions.
-- P1 is the serve position; P4, P3, and P2 are the front row from left to right;
-- and P5 and P6 are right and middle backrow players respectively.
data Position = P1 | P2 | P3 | P4 | P5 | P6 deriving (Show)

-- The server serves from position P1.
servicePosition :: Position
servicePosition = P1

-- The position of the setter in "rotation 1" of a 5:1 is the service position, which is P1.
setterBasePosition :: Position
setterBasePosition = servicePosition

-- A function to compute the next position of a current position after 1 rotation.
-- For example: If you are in P2 and you rotate, you're now in P1!
-- 'rotate' is a function that takes a current 'Position' and returns the next 'Position'.
rotate :: Position -> Position
rotate p = case p of
  P1 -> P6 -- P1 goes to P6.
  P6 -> P5 -- P6 goes to P5.
  P5 -> P4 -- and so on...
  P4 -> P3
  P3 -> P2
  P2 -> P1 -- And finally, P2 goes to P1.

-- Similarly, we can define a function to compute the previous position;
-- we will call it 'reverseRotate' as it does one rotation in reverse.
-- We could enumerate all the positions like we did in 'rotate', but we know
-- that rotating 1 position backwards is the same as rotating 5 positions forward.
-- So let's use our 'rotate' function 5 times!
reverseRotate :: Position -> Position
reverseRotate p = rotate (rotate (rotate (rotate (rotate p))))

-- Here is a function to find the opposite position given a position.
-- It's just 3 rotations.
opposite :: Position -> Position
opposite p = rotate (rotate (rotate p))

-- A test of what we've built so far.
main :: IO ()
main = do
  putStrLn "Hi volleyball positions!"
  putStrLn $ "In volleyball, the service position is " <> show servicePosition <> "."
  putStrLn $ "The setter's base position is " <> show setterBasePosition <> "."
  putStrLn $ "P1 rotates to " <> show (rotate P1) <> " in the next rotation."
  putStrLn $ "P1 came from " <> show (reverseRotate P1) <> " in the previous rotation."
  putStrLn $ show (opposite P1) <> " is opposite P1."
