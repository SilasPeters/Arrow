module Main ( main ) where

import Algebra
import Model
import Interpreter
import Lexer
import Parser

import Control.Monad ( void )
import ParseLib.Abstract ( parse )
import Data.Char ( toUpper )

-- Exercise 11
interactive :: Environment -> ArrowState -> IO ()
interactive = iterateProgram interactiveAlgebra

batch :: Environment -> ArrowState -> (Space, Pos, Heading)
batch = iterateProgram batchAlgebra

data IterateProgramAlgebra r = IterateGameAlgebra
  { onSuccess :: Step -> r
  , onFail    :: Step -> String -> r -- here Step is the previous step
  , promptPro :: IO ()
  , printStep :: Step -> IO ()
  }

interactiveAlgebra :: IterateProgramAlgebra (IO ())
interactiveAlgebra = IterateGameAlgebra
  (\_ -> putStrLn "Program terminated successfully!")
  (const $ putStrLn . (++) "Program failed... Reason: ")
  promptProgress
  (\(Ok (ArrowState sp _ _ _)) -> printBoard sp)

batchAlgebra :: IterateProgramAlgebra (Space, Pos, Heading)
batchAlgebra = IterateGameAlgebra
  (\(Done sp po he)   -> (sp, po, he))
  (\(Ok (ArrowState sp po he _)) _ -> (sp, po, he))
  (return ())
  (const $ return ())

iterateProgram :: IterateProgramAlgebra r -> Environment -> ArrowState -> r
iterateProgram alg env arr
  = iter alg (Ok arr) (step env arr)
  where
    iter :: IterateProgramAlgebra r -> Step -> Step -> r -- previous step -> resulting Step -> r
    iter (IterateGameAlgebra onSuccess printFail promptPro printStep) _    s@Done { }  = onSuccess s
    iter alg@(IterateGameAlgebra onSuccess printFail promptPro printStep) _    s@(Ok ar)   = promptPro >> printStep s >> iter alg (step env ar) s
    iter alg@(IterateGameAlgebra onSuccess printFail promptPro printStep) prev s@(Fail er) = printFail prev er >> return s

promptProgress :: IO ()
promptProgress = putStr "Press any key to continue..." >> void getChar -- disregards any input

printBoard :: Space -> IO () -- TODO print the whole Step if possible
printBoard = putStrLn . printSpace



promptInput :: String -> (String -> a) -> IO a
promptInput str f = putStr str >> (f <$> getLine)

main :: IO ()
main = do
  spaceIn   <- promptInput ".space path (like \".\\examples\\Maze.space\"): " readFile -- TODO parse using 'run'
  space     <- fst . head . parse parseSpace <$> spaceIn
  environIn <- promptInput ".program path (like \".\\examples\\Maze.space\"): " readFile
  environ   <- toEnvironment <$> environIn
  pos       <- promptInput "What is the ship's initial position? (x,y) > " read -- TODO bounds check?
  heading   <- promptInput "What is the ship's initial heading? [N|E|S|W]> " $ read . map toUpper

  let initialState = ArrowState space pos heading [CIdent "start"]

  modeInput <- putStr "What mode would you like to try now? [interactive|batch]" >> getLine
  case modeInput of
    "interactive" -> interactive environ initialState
    "batch"       -> print $ batch environ initialState
    _             -> putStrLn "Unknown mode, aborting..."

