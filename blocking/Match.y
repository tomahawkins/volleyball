{
module Match
  ( Match          (..)
  , Set            (..)
  , Volley         (..)
  , Attack         (..)
  , Team           (..)
  , parseMatch
  ) where
}

%name sets
%tokentype { Char }
%error { parseError }

%expect 0

%token

"."           { '.' }
"a"           { 'a' }
"b"           { 'b' }
"j"           { 'j' }
"k"           { 'k' }
"s"           { 's' }
"w"           { 'w' }
"m"           { 'm' }
"t"           { 't' }
"x"           { 'x' }
"0"           { '0' }
"1"           { '1' }
"2"           { '2' }
"3"           { '3' }

%%

Sets :: { [Set] }
:                { [] }
| Set Sets1      { $1 : $2 }

Sets1 :: { [Set] }
:               { [] }
| Set Sets      { swapVolleys $1 : $2 }
 
Set :: { Set }
: Volleys Win { volleys' $1 $2 }

Win :: { Team }
: Team "w" "." { $1 }

Team :: { Team }
: "a" { A }
| "b" { B }
| "j" { A }
| "k" { B }

Volleys :: { [Volley'] }
:                 { [] }
| Volleys Volley  { $1 ++ [$2] }

Volley :: { Volley' }
: Team "s" Attacks "."         { Volley' $1 $3 False }
| Team "s" Attacks "." "x" "." { Volley' $1 $3 True  }

Attacks :: { [Attack] }
:                { [] }
| Attacks Attack { $1 ++ [$2] }

Attack :: { Attack }
: Team Blockers { Attack $1 $2 }

Blockers :: { Int }
: "0" { 0 }
| "1" { 1 }
| "2" { 2 }
| "3" { 3 }

{

data Match   = Match String String [Set]                    -- A match is the team names and a list of sets.
type Set     = [Volley]                                     -- A set is a sequence of volleys.
data Volley' = Volley' Team [Attack] Bool    deriving Show  -- A volley is a side that serves and a sequence of attacks and if the teams swich sides after volley.
data Volley  = Volley  Team [Attack] Team    deriving Show  -- A volley is a side that serves, a sequence of attacks, and who wins.
data Attack  = Attack  Team Int              deriving Show  -- Attacking side and number of blockers.
data Team    = A | B                      deriving (Show, Eq)  -- Team A or side B.

parseMatch :: String -> Match
parseMatch a = Match teamA teamB $ swapTeams $ sets $ concat game
  where
  teamA : "vs" : teamB : game = words . unlines . map (takeWhile (/= '#')) $ lines a

swapTeams :: [Set] -> [Set]
swapTeams a = case a of
  [a]             -> [a]
  [a, b]          -> [a, swapSet b]
  [a, b, c]       -> [a, swapSet b, c]
  [a, b, c, d]    -> [a, swapSet b, c, swapSet d]
  [a, b, c, d, e] -> [a, swapSet b, c, swapSet d, swapSet e]
  _ -> error "Too many sets."

swapSet :: Set -> Set
swapSet = swapVolleys

swapVolleys :: [Volley] -> [Volley]
swapVolleys = map volley
  where
  volley :: Volley -> Volley
  volley (Volley a b c) = Volley (team a) (map attack b) (team c)

  attack :: Attack -> Attack
  attack (Attack a b) = Attack (team a) b

  team :: Team -> Team
  team A = B
  team B = A

parseError :: String -> a
parseError a = case a of
  [] -> error "Parse error: no tokens left to parse."
  a  -> error $ "Parse error: unexpected token sequence:  " ++ a

volleys' :: [Volley'] -> Team -> [Volley]
volleys' a winner = f a
  where
  f :: [Volley'] -> [Volley]
  f a = case a of
    [] -> []
    Volley' a b _ : []                       -> Volley a b winner : []
    Volley' a b x : d@(Volley' c _ _) : rest -> Volley a b c : (if x then swapVolleys else id) (f (d : rest))

}
