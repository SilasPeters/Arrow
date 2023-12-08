module Model where

-- Exercise 1
data Token = TArrow | TDot | TComma
  | TGo | TTake | TMark | TNothing | TTurn
  | TCase | TOf | TEnd
  | TLeft | TRight | TFront | TDotComma
  | TEmpty | TLambda | TDebris | TAsteroid | TBoundary | TUnderscore
  | TIdent String
  deriving Show

-- Exercise 2
data Program = Program deriving Show
