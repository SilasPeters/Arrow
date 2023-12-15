{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
module Algebra where

import Model
import Data.List ( find )
import Data.Maybe ( isJust )

import Prelude hiding ( take )
import Text.Printf (FormatParse(fpChar))
import Data.Bifunctor

-- Exercise 5
type ProgramAlgebra r = [Rule] -> r

foldProgram :: ProgramAlgebra r -> Program -> r
foldProgram alg (Program rs) = alg rs;

foldProgram' :: ProgramAlgebra' p r c a -> Program -> p
foldProgram' (Prog aprogram arule acommands acase acommand aalts aalt) = fp
  where
    fp (Program rs) = aprogram (frs rs)
    frs = map (\(Rule n cs) -> arule n (fcs cs))
    fcs cs = acommands (map fc cs)
    fc (CCase d as) = acase d (fas as)
    fc c = acommand c
    fas as = aalts (map fa as)
    fa (Alt p cs) = aalt p (fcs cs)

data ProgramAlgebra' p r c a = Prog {
    aProgram :: [r] -> p
  , aRule :: String -> c -> r
  , aCommands :: [c] -> c
  , aCase :: Dir -> a -> c
  , aCommand :: Command -> c
  , aAlts :: [a] -> a
  , aAlt :: Pat -> c -> a
  }

-- Exercise 6

checkProgram :: Program -> Bool
checkProgram p = caseExpressionsExhausted p && noCallsToUndefinedRules p && uniqueRules' p && startRuleExists' p

caseExpressionsExhausted :: Program -> Bool
caseExpressionsExhausted = foldProgram' caseExpressionsExhaustedAlgebra

noCallsToUndefinedRules :: Program -> Bool
noCallsToUndefinedRules = foldProgram' noCallsToUndefinedRulesAlgebra

uniqueRules' :: Program -> Bool
uniqueRules' = foldProgram' uniqueRulesAlgebra'

startRuleExists' :: Program -> Bool
startRuleExists' = foldProgram' startRuleExistsAlgebra'

noCallsToUndefinedRulesAlgebra :: ProgramAlgebra' Bool (String,[String]) [String] [String]
noCallsToUndefinedRulesAlgebra = Prog
  ((\(is, ss) -> all (`elem` is) ss).second concat.unzip)
  (,)
  concat
  (\d a -> a)
  (\c -> case c of
    CIdent s -> [s]
    _ -> [])
  concat
  (\_ c -> c)

caseExpressionsExhaustedAlgebra :: ProgramAlgebra' Bool Bool Bool ([Pat], Bool)
caseExpressionsExhaustedAlgebra = Prog
  and
  (\ _ x -> x)
  and
  (\_ (ps,b) -> complete ps && b)
  (const True)
  (bimap concat and.unzip)
  (\p c -> ([p], c))

complete :: [Pat] -> Bool
complete ps = elem PUnderscore ps || (elem PEmpty ps && elem PLambda ps && elem PDebris ps && elem PAstroid ps && elem PBoundary ps)

uniqueRulesAlgebra' :: ProgramAlgebra' Bool String Int Int
uniqueRulesAlgebra' = Prog
  (snd . foldl (\(ss, b) s -> (s:ss, b && notElem s ss)) ([],True))
  const
  (const 0)
  (const $ const 0)
  (const 0)
  (const 0)
  (const $ const 0)

startRuleExistsAlgebra' :: ProgramAlgebra' Bool Bool Int Int
startRuleExistsAlgebra' = Prog
  or
  (\s _ -> s == "start")
  (const 0)
  (const $ const 0)
  (const 0)
  (const 0)
  (const $ const 0)








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






