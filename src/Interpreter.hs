module Interpreter where

import ParseLib.Abstract
import Prelude hiding ((<$), ($>), (<*), (*>), sequence)

import Data.Map (Map, empty, insert, (!), (!?), foldrWithKey, lookup)
import qualified Data.Map as L

import Data.Char (isSpace, GeneralCategory (Space))
import Control.Monad (replicateM)

import Lexer
import Parser
import Model
import Algebra
import GHC.TypeLits (SomeChar(SomeChar))
import Data.Sequence (chunksOf, lookup)
import System.Environment (getEnvironment)
import Text.Read (Lexeme(String))
import Data.Maybe (fromMaybe)


data Contents  =  Empty | Lambda | Debris | Asteroid | Boundary deriving (Eq, Ord, Show)

type Size      =  Int
type Pos       =  (Int, Int)
type Space     =  Map Pos Contents

add :: Pos -> Pos -> Pos
add (y1, x1) (y2,x2) = (y1 + y2,x1 +x2)
 -- NOTE THE ASSIGNMENT SEEMS TO CONFUSE X AND Y IN THE EXAMPLE OF PAGE 9,
 -- AND THUS WE GLOBALLY TREAT X AS Y AND Y AS X.

-- | Parses a space file, such as the ones in the examples folder.
parseSpace :: Parser Char Space
parseSpace = do
    (mr, mc) <- parenthesised ((,) <$> natural <* symbol ',' <*> natural)
                <* spaces
    -- read |mr + 1| rows of |mc + 1| characters
    css      <- replicateM (mr + 1) (replicateM (mc + 1) contents)
    -- convert from a list of lists to a finite map representation
    return $ L.fromList $ concat $
            zipWith (\r cs ->
              zipWith (\c d -> ((r, c), d)) [0..] cs) [0..] css
  where
    spaces :: Parser Char String
    spaces = greedy (satisfy isSpace)

    contents :: Parser Char Contents
    contents = choice (map (\(f,c) -> f <$ symbol c) contentsTable)
      <* spaces


-- | Conversion table
contentsTable :: [ (Contents, Char)]
contentsTable =  [ (Empty   , '.' )
                 , (Lambda  , '\\')
                 , (Debris  , '%' )
                 , (Asteroid, 'O' )
                 , (Boundary, '#' )]

contentsMap :: Map Contents Char
contentsMap = foldr (uncurry insert) e contentsTable
  where
    e :: Map Contents Char
    e = Data.Map.empty


-- Exercise 7
printSpace :: Space -> String
printSpace s = show size ++ "\n" ++ spaceLines s size size
  where
    size :: Pos
    size = foldrWithKey (\(nx,ny) _ (x,y) -> (max x nx, max y ny)) (0,0) s

spaceLines :: Space -> Pos -> Pos -> String
spaceLines s (0,0) _            = [contentsMap !  (s ! (0,0))]
spaceLines s (y,0) size@(_,mx)  =  spaceLines s (y-1,mx) size ++ "\n" ++ [contentsMap !  (s ! (y,0))]
spaceLines s (y,x) size         =  spaceLines s (y,x-1) size ++ [contentsMap !  (s ! (y,x))]

-- These three should be defined by you
type Ident = String
type Commands = [Command]
data Heading = N | E | S | W
  deriving (Show, Read)

type Environment = Map Ident Commands

type Stack       =  Commands
data ArrowState  =  ArrowState
  { getSpace   :: Space
  , getPos     :: Pos
  , getHeading :: Heading
  , getStack   :: Stack
  }
  deriving (Show)

data Step =  Done  Space Pos Heading
          |  Ok    ArrowState
          |  Fail  String
  deriving (Show)

-- | Exercise 8
toEnvironment :: String -> Environment
toEnvironment s = if checkProgram program
  then
    foldProgram' getEnvironmentAlgebra program
  else
    undefined -- The assignment does not specify what to do in this case
  where
    program = Program $ parser $ alexScanTokens s

getEnvironmentAlgebra :: ProgramAlgebra' Environment (Ident, Commands) [Command] [Alt]
getEnvironmentAlgebra = Prog
  (foldr (uncurry insert) Data.Map.empty)
  (,)
  concat
  (\d as -> [CCase d as])
  (: [])
  concat
  (\p cs -> [Alt p cs])

-- | Exercise 9
headingToPos :: Heading -> (Int,Int)
headingToPos h = case h of
  N -> (-1,0)
  E -> (0,1)
  S -> (1,0)
  W -> (0,-1)

rotateheading :: Heading -> Dir -> Heading
rotateheading h d = case d of
  DLeft -> case h of
    N -> W
    E -> N
    S -> E
    W -> S
  DRight -> case h of
    N -> E
    E -> S
    S -> W
    W -> N
  _ -> h

moveHeading :: Pos -> Heading -> Pos
moveHeading p h = p `add` headingToPos h

match:: Pat -> Contents -> Bool
match p c = case p of
  PEmpty -> c == Empty
  PLambda -> c == Lambda
  PDebris -> c == Debris
  PAstroid -> c == Asteroid
  PBoundary -> c == Boundary
  PUnderscore -> True

step :: Environment -> ArrowState -> Step
step env (ArrowState sp p h []) = Done sp p h
step env state@(ArrowState sp p h (c:cs)) = case c of
  CTake     -> Ok $ ArrowState (insert p Empty sp) p h cs
  CMark     -> Ok $ ArrowState (insert p Lambda sp) p h cs
  CNothing  -> Ok $ ArrowState sp p h cs
  CTurn d   -> Ok $ ArrowState sp p (rotateheading h d) cs
  CCase _ _ -> ccase env state
  CGo       -> case fromMaybe Boundary (sp !? moveHeading p h) of
    c -> if (c == Asteroid) || (c == Boundary)
      then Ok $ ArrowState sp p h cs
      else Ok $ ArrowState sp (moveHeading p h) h cs
  CIdent s  -> case env !? s of
    Just coms -> Ok $ ArrowState sp p h (coms ++ cs)
    Nothing   -> Fail $ s ++ " Not a command"

ccase :: Environment -> ArrowState -> Step
ccase env (ArrowState sp pos h ((CCase d []):cs)) = Fail "non exhaustive pattern"
ccase env (ArrowState sp pos h ((CCase d ((Alt pat coms):as)):cs)) =
  case fromMaybe Boundary (sp !? moveHeading pos (rotateheading h d)) of
    c -> if match pat c
      then Ok $ ArrowState sp pos h (coms ++ cs)
      else ccase env (ArrowState sp pos h (CCase d as:cs))



