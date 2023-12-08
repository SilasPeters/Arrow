{
module Lexer where

import Model
}

%wrapper "basic"

$digit = 0-9
$letter = [a-zA-Z]

tokens :-
  \-\>                     { const TArrow }
  \.                       { const TDot }
  \,                       { const TComma }

  go                       { const TGo }
  take                     { const TTake }
  mark                     { const TMark }
  nothing                  { const TNothing }
  turn                     { const TTurn }

  case                     { const TCase }
  of                       { const TOf }
  end                      { const TEnd }

  left                     { const TLeft }
  right                    { const TRight }
  front                    { const TFront }
  \;                       { const TDotComma }

  Empty                    { const TEmpty }
  Lambda                   { const TLambda }
  Debris                   { const TDebris }
  Asteroid                 { const TAsteroid }
  Boundary                 { const TBoundary }
  \_                       { const TUnderscore }

  [$letter $digit \+ \-]+  { TIdent }

  _                        { const (TIdent "_") }

