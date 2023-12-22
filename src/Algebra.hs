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
foldProgram' :: ProgramAlgebra' p r c a -> Program -> p
foldProgram' (Prog aprogram arule acommands acase acommand aalts aalt) = fp
  where
    fp (Program rs) = aprogram (frs rs)
    frs             = map (\(Rule n cs) -> arule n (fcs cs))
    fcs cs          = acommands (map fc cs)
    fc (CCase d as) = acase d (fas as)
    fc c            = acommand c
    fas as          = aalts (map fa as)
    fa (Alt p cs)   = aalt p (fcs cs)

data ProgramAlgebra' p r c a = Prog {
    aProgram  :: [r] -> p
  , aRule     :: String -> c -> r
  , aCommands :: [c] -> c
  , aCase     :: Dir -> a -> c
  , aCommand  :: Command -> c
  , aAlts     :: [a] -> a
  , aAlt      :: Pat -> c -> a
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
