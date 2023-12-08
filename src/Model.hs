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
newtype Program = Program [Rule]
  deriving Show

data Rule = Rule String [Command]
  deriving Show

data Command = CGo | CTake | CMark | CNothing
             | CTurn Dir | CCase Dir [Alt]
             | CIdent String
  deriving Show

data Dir = Left | Right | Front
  deriving Show

data Alt = Alt Pat [Command]
  deriving Show

data Pat = PEmpty | PLambda | PDebris | PAstroid | PBoundary | PUnderscore
  deriving Show

