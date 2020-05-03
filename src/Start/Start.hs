module Start.Start where

import Bnfc.LexHpl ( Token )
import Bnfc.ParHpl
import Bnfc.PrintHpl ( Print )
import Bnfc.ErrM
import Bnfc.AbsHpl
import StaticCheck.StaticCheck ( staticCheck )
import StaticCheck.Format
import Run.Run ( run )
import Debug.Trace ( trace )

runFile :: FilePath -> IO ()
runFile = runFileAux pProgram

type ParseFun a = [Token] -> Err a

myLLexer = myLexer

runFileAux :: ParseFun Program -> FilePath -> IO ()
runFileAux p f = putStrLn f >> readFile f >>= runAux p

runAux :: ParseFun Program -> String -> IO ()
runAux p s = let ts = myLLexer s in case p ts of
         Bad s      -> do 
                        putStrLn "\nParse Failed... TODO\n"
                        print ts
                        putStrLn s
         Ok tree    -> do
                        putStrLn "\nParse Successful!"
                        checkAndRunTree tree 

checkAndRunTree :: Program -> IO ()
checkAndRunTree x = tryRun $ staticCheck x

tryRun :: Err NProgramFormat -> IO ()
tryRun (Bad err) = putStrLn $ "TODO: make it nicer " ++ err
tryRun (Ok tree) = run tree
                           
