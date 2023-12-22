module Main ( main ) where

import Algebra
import Model
import Interpreter
import Lexer
import Parser

import Control.Monad ( void )
import ParseLib.Abstract ( parse )
import Data.Char ( toUpper )
import System.IO ( hFlush, stdout )

-- Exercise 11
interactive :: Environment -> ArrowState -> IO ()
interactive env = iter . Ok
  where
    iter :: Step -> IO ()
    iter (Done sp _ _) = putStrLn "Program terminated successfully!" --TODO remove the prnt here
    iter (Ok ar)       = printState ar >> promptProgress >> iter (step env ar)
    iter (Fail er)     = putStrLn ("Program failed... Reason: " ++ er)


batch :: Environment -> ArrowState -> (Space, Pos, Heading)
batch env arr = (\(Done sp po he) -> (sp, po, he)) $ iter undefined (Ok arr)
  where
    iter :: Step -> Step -> Step -- previous state -> current state -> resulting state
    iter _                            s@(Ok ar)  = iter s (step env ar)
    iter _                            s@Done { } = s
    iter (Ok (ArrowState sp po he _)) s@Fail { } = Done sp po he -- Assumes that this is meant with "no more steps to be taken"


promptProgress :: IO ()
promptProgress = putStr "Press any key to continue... " >> hFlush stdout >> void getChar >> putStrLn "" -- disregards any input

printState :: ArrowState -> IO () -- TODO print the whole Step if possible
printState (ArrowState sp pos he cs) = do
  putStrLn $ printSpace sp
  putStrLn ""
  putStr "Pos: " >> print pos
  putStr "Heading: " >> print he
  putStr "Call-stack: " >> print cs

promptInput :: String -> (String -> a) -> IO a
promptInput str f = putStr str >> hFlush stdout >> (f <$> getLine)

main :: IO ()
main = do
  spaceIn   <- promptInput ".space path (like \"./examples/AddInput.space\"): " readFile -- TODO parse using 'run'
  space     <- fst . head . parse parseSpace <$> spaceIn
  environIn <- promptInput ".program path (like \"./examples/Add.arrow\")   : " readFile
  environ   <- toEnvironment <$> environIn
  pos       <- promptInput "What is the ship's initial position? (x,y)    > " read -- TODO bounds check?
  heading   <- promptInput "What is the ship's initial heading? [N|E|S|W] > " $ read . map toUpper

  let initialState = ArrowState space pos heading [CIdent "start"]

  modeInput <- promptInput "What mode would you like to try now? [interactive|batch] > " id
  case modeInput of
    "interactive" -> interactive environ initialState
    "batch"       -> print $ batch environ initialState
    _             -> putStrLn "Unknown mode, aborting..."

