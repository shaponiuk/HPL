module StaticCheck.CheckMainFunction where

import Util.Util
import StaticCheck.Format
import Bnfc.ErrM

checkMainFunction :: ProgramFormat -> Err ()
checkMainFunction (SITList funDefs _ _) = do
    main <- findMain funDefs
    checkMain main

findMain :: [FFunctionDef] -> Err FFunctionDef
findMain [] = fail "function main not found"
findMain (fd@(SusFFunctionDef (NonSusFFunctionDef _ _ "main" _ _)):_) = return fd
findMain (NonSusFFunctionDef (Just pos) _ "main" _ _:_) = fail $ "function main at " ++ show pos ++ " is not suspended"
findMain (_:fds) = findMain fds

checkMain :: FFunctionDef -> Err ()
checkMain (SusFFunctionDef (NonSusFFunctionDef (Just pos) (FTypeB _ "Int" []) _ [] _)) = return ()
checkMain (SusFFunctionDef (NonSusFFunctionDef (Just pos) (FTypeB _ "Int" []) _ args _)) = 
    fail $ "function main at " ++ show pos ++ " should have 0 arguments but has " ++ show (length args)
checkMain (SusFFunctionDef (NonSusFFunctionDef (Just pos) t _ _ _)) = 
    fail $ "function main at " ++ show pos ++ " should be of the type Int () but is of the type " ++ show t
checkMain _ = undefined