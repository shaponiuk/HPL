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
import Messages.Messages

runFile :: FilePath -> IO ()
runFile = runFileAux pProgram

type ParseFun a = [Token] -> Err a

myLLexer = myLexer

runFileAux :: ParseFun (Program (Maybe (Int, Int))) -> FilePath -> IO ()
runFileAux p f = readFile f >>= runAux p

runAux :: ParseFun (Program (Maybe (Int, Int))) -> String -> IO ()
runAux p s = let ts = myLLexer s in case p ts of
        Bad str -> do 
            putStrLn "\nParse Failed\n"
            putStrLn str
        Ok tree ->
            checkAndRunTree tree 
        

checkAndRunTree :: Program (Maybe (Int, Int)) -> IO ()
checkAndRunTree x = tryRun $ staticCheck x

tryRun :: Err (NProgramFormat, [String]) -> IO ()
tryRun (Bad err) = putStrLn $ "ERROR: " ++ err
tryRun (Ok (tree, warnings)) = do
    printWarnings warnings
    run tree
                           
