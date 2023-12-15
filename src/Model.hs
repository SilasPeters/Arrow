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

data Rule = Rule { name :: String
                 , cmds :: [Command] }
  deriving Show

data Command = CGo | CTake | CMark | CNothing
             | CTurn Dir | CCase Dir [Alt]
             | CIdent String
  deriving Show

data Dir = DLeft | DRight | DFront
  deriving Show

data Alt = Alt Pat [Command]
  deriving Show

data Pat = PEmpty | PLambda | PDebris | PAstroid | PBoundary | PUnderscore
  deriving (Eq, Show)

