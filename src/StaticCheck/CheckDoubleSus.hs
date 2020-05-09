module StaticCheck.CheckDoubleSus where

import StaticCheck.Format
import Bnfc.ErrM

checkDoubleSus :: ProgramFormat -> Err ProgramFormat
checkDoubleSus pf@(SITList functionDefs _ _) = do
    checkDoubleSusFunctionDefs functionDefs
    return pf

checkDoubleSusFunctionDef :: FFunctionDef -> Bool
checkDoubleSusFunctionDef NonSusFFunctionDef{} = True
checkDoubleSusFunctionDef (SusFFunctionDef NonSusFFunctionDef{}) = True
checkDoubleSusFunctionDef _ = False

checkDoubleSusFunctionDefs :: [FFunctionDef] -> Err ()
checkDoubleSusFunctionDefs [] = return ()
checkDoubleSusFunctionDefs (fdef:fdefs) =
    if checkDoubleSusFunctionDef fdef
        then checkDoubleSusFunctionDefs fdefs
        else fail $ "multi sus definition of " ++ getFunctionName fdef