{
module Touches (parseTouches) where
}

%name match
%tokentype { Char }
%error { parseError }

%expect 0

%token

"w"           { 'w' }
"l"           { 'l' }
"d"           { 'd' }
"o"           { 'o' }
"a"           { 'a' }
"f"           { 'f' }
"t"           { 't' }
"m"           { 'm' }
"1"           { '1' }
"2"           { '2' }
"3"           { '3' }

%%

Attacks :: { [(Bool, Bool)] }
:                { [] }
| Attacks Attack { $1 ++ [$2] }

Attack :: { (Bool, Bool) }
: Int Touch Result { ($2, $3) }

Int :: { () }
:      { () }
| "1"  { () }
| "2"  { () }
| "3"  { () }

Touch :: { Bool }
:      { False }
| "t"  { True  }
| "m"  { False }

Result :: { Bool }
: "w"      { True  }
| "l"      { False }
| "d" "a"  { True  }
| "d" "f"  { False }
| "o" "a"  { False }
| "o" "f"  { True  }

{
parseError :: String -> a
parseError a = case a of
  [] -> error "Parse error: no tokens left to parse."
  a  -> error $ "Parse error: unexpected token sequence:  " ++ a

-- | A list of attacks: (if blockers touched the ball, if outcome gave advantage to blocking team)
parseTouches :: String -> [(Bool, Bool)]
parseTouches = match . concat . words . unlines . map (takeWhile (/= '#')) . lines
}

