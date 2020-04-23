module Start.Start where

import Bnfc.LexHpl ( Token )
import Bnfc.ParHpl
import Bnfc.PrintHpl ( Print )
import Bnfc.ErrM
import StaticCheck.StaticCheck ( staticCheck )
import Run.Run ( actualRun )

runFile :: FilePath -> IO ()
runFile x = runFileAux pProgram x

type ParseFun a = [Token] -> Err a

myLLexer = myLexer

runFileAux :: (Print a, Show a) => ParseFun a -> FilePath -> IO ()
runFileAux p f = putStrLn f >> readFile f >>= run p

run :: (Print a, Show a) => ParseFun a -> String -> IO ()
run p s = let ts = myLLexer s in case p ts of
         Bad s      -> do 
                        putStrLn "\nParse Failed...\n"
                        putStrLn "Tokens:"
                        putStrLn $ show ts
                        putStrLn s
         Ok tree    -> do
                        putStrLn "\nParse Successful!"
                        checkAndRunTree tree 

checkAndRunTree x :: Program -> IO ()
checkAndRunTree x = tryRun $ staticCheck x

tryRun :: Err Program -> IO ()
tryRun (Left err) = putStrLn $ "TODO: make it nicer " ++ err
tryRun (Right tree) = actualRun tree
                           
