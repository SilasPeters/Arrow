module Algebra where

import Model
import Data.List ( find )
import Data.Maybe ( isJust )

import Prelude hiding ( take )

-- Exercise 5
type ProgramAlgebra r = [Rule] -> r

foldProgram :: ProgramAlgebra r -> Program -> r
foldProgram alg (Program rs) = alg rs;

-- Exercise 6

type RuleAlgebra r = String -> [Command] -> r

foldRule :: RuleAlgebra r -> Rule -> r
foldRule alg (Rule s cmds) = alg s cmds

data CommandAlgebra r = CommandAlgebra { 
   go :: r
  ,take :: r
  ,mark :: r
  ,nothing :: r
  ,turn :: Dir -> r
  ,goCase :: Dir -> [Alt] -> r
  ,ident :: String -> r 
  }

foldCommand :: CommandAlgebra r -> Command -> r
foldCommand alg CGo              = go alg
foldCommand alg CTake            = take alg
foldCommand alg CMark            = mark alg
foldCommand alg CNothing         = nothing alg
foldCommand alg (CTurn dir)      = turn alg dir
foldCommand alg (CCase dir alts) = goCase alg dir alts
foldCommand alg (CIdent s)       = ident alg s

data DirAlgebra r = DirAlgebra { 
   left :: r
  ,right :: r
  ,front :: r 
  }

foldDir :: DirAlgebra r -> Dir -> r
foldDir alg DLeft  = left alg
foldDir alg DRight = right alg
foldDir alg DFront = front alg

type Altgebra r = Pat -> [Command] -> r

foldAlt :: Altgebra r -> Alt -> r
foldAlt alg (Alt pat cmds) = alg pat cmds

data PatAlgebra r = PatAlgebra { 
    empty :: r
   ,lambda :: r
   ,debris :: r
   ,astroid :: r
   ,boundary :: r
   ,underscore :: r 
   }

foldPat :: PatAlgebra r -> Pat -> r
foldPat alg PEmpty      = empty alg
foldPat alg PLambda     = lambda alg
foldPat alg PDebris     = debris alg
foldPat alg PAstroid    = astroid alg
foldPat alg PBoundary   = boundary alg
foldPat alg PUnderscore = underscore alg



noCallsToUndefinedRulesAlgebra :: ProgramAlgebra Bool
noCallsToUndefinedRulesAlgebra = undefined

noCallsToUndefinedRules :: Program -> Bool
noCallsToUndefinedRules = undefined

uniqueRulesAlgebra :: ProgramAlgebra Bool
uniqueRulesAlgebra = go []
  where
    go :: [String] -> ProgramAlgebra Bool
    go _  []     = True
    go ac (r:rs) | name r `elem` ac = False
                 | otherwise = go (name r:ac) rs

uniqueRules :: Program -> Bool
uniqueRules = foldProgram uniqueRulesAlgebra

startRuleExistsAlgebra :: ProgramAlgebra Bool
startRuleExistsAlgebra = isJust . find ((==) "start" . name)

startRuleExists :: Program -> Bool
startRuleExists = foldProgram startRuleExistsAlgebra

caseExpressionsExhaustedAlgebra :: CommandAlgebra Bool
caseExpressionsExhaustedAlgebra = undefined

caseExpressionsExhausted :: Command -> Bool
caseExpressionsExhausted = undefined

checkProgram :: Program -> Bool
checkProgram = undefined
