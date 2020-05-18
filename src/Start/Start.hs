module Start.Start where

import Bnfc.LexHpl ( Token )
import Bnfc.ParHpl
import Bnfc.PrintHpl
import Bnfc.ErrM
import Bnfc.AbsHpl
import StaticCheck.StaticCheck ( staticCheck )
import StaticCheck.Format
import Run.Run ( run )
import Util.Util

runFile :: FilePath -> IO ()
runFile = runFileAux pProgram

type ParseFun a = [Token] -> Err a

myLLexer = myLexer

runFileAux :: ParseFun (Program (Maybe (Int, Int))) -> FilePath -> IO ()
runFileAux p f = readFile f >>= runAux p

runAux :: ParseFun (Program (Maybe (Int, Int))) -> String -> IO ()
runAux p s = let ts = myLLexer s in case p ts of
         Bad s      -> do 
                        putStrLn "\nParse Failed... TODO\n"
                        print ts
                        putStrLn s
         Ok tree    ->
                        checkAndRunTree tree 

checkAndRunTree :: Program (Maybe (Int, Int)) -> IO ()
checkAndRunTree x = tryRun $ staticCheck x

tryRun :: Err NProgramFormat -> IO ()
tryRun (Bad err) = putStrLn $ "ERROR: " ++ err
tryRun (Ok tree) = run tree
                           
