{
module VolleyParse (volley) where

import Data.List

import Match

}

%name      volley
%tokentype { String }
%error     { parseError }

%expect 0

%token

"("          { "(" }
")"          { ")" }
"["          { "[" }
"]"          { "]" }
";"          { ";" }
"Bad"        { "Bad" }
"set"        { "set" }
"by"         { "by"  }
"from"       { "from"     }
"block"      { "block"    }
"Kill"       { "Kill"     }
"Attack"     { "Attack"   }
"Service"    { "Service"  }
"error"      { "error"    }
"handling"   { "handling" }
"ace"        { "ace"      }
"Ball"       { "Ball"     }
"Point"      { "Point"    }
"to"         { "to"       }
"official"   { "official" }
"awarded"    { "awarded"  }
name         { _ }

%%

Volley :: { (Maybe Name, Volley) }
: Server "Bad" "set" "by" MaybeName                                              { ($1, BadSet $5) }
| Server "Kill" "by" MaybeName                                                   { ($1, KillBy $4 Nothing   Nothing   ) }
| Server "Kill" "by" MaybeName "(" "from" Name ")"                               { ($1, KillBy $4 (Just $7) Nothing   ) }
| Server "Kill" "by" MaybeName                     "block" "error" "by" Name     { ($1, KillBy $4 Nothing   (Just $8) ) }
| Server "Kill" "by" MaybeName "(" "from" Name ")" "block" "error" "by" Name     { ($1, KillBy $4 (Just $7) (Just $12)) }
| Server "Attack" "error" "by" MaybeName                                         { ($1, AttackError $5 []) }
| Server "Attack" "error" "by" MaybeName "(" "block" "by" Names ")"              { ($1, AttackError $5 $9) }
| Server "Service" "error"                                                       { ($1, ServiceError) }
| Server "Service" "ace" "(" MaybeName ")"                                       { ($1, ServiceAce $5) }
| Server "Ball" "handling" "error" "by" MaybeName                                { ($1, BallHandlingError $6) }
| "Point" "awarded" "by" "official" "to" MaybeName                               { (Nothing, PointAwarded) }
| "Point" "awarded" "by" "official" "to" Name "(" Name ")"                       { (Nothing, PointAwarded) }

Server :: { Maybe Name }
: "[" Name "]"  { Just $2 }

MaybeName :: { Maybe Name }
:       { Nothing }
| Name  { Just $1 }

Name :: { Name }
:          name  { $1 }
| Name     name  { $1 ++ " " ++ $2 }

Names :: { [Name] }
:           Name  { [$1] }
| Names ";" Name  { $1 ++ [$3] }

{
parseError :: [String] -> a
parseError a = case a of
  [] -> error "VolleyParse error: no tokens left to parse."
  a  -> error $ "VolleyParse error: unexpected sequence: \"" ++ intercalate " " a ++ "\""
}

