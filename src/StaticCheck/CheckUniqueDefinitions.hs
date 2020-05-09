module StaticCheck.CheckUniqueDefinitions where

import StaticCheck.Format
import Bnfc.ErrM
import Util.Util
import Data.Map
import Control.Monad.State
import Control.Monad.Trans.State as ST
import Control.Monad.Trans.Except
import Control.Monad.Trans.Identity

newtype UniqueDefS = UniqueDefS (Map String [(Bool, String, FType, [FPatternMatch])])
    deriving (Show)

hasDifferentSusOrDifferentTypeOrDifferentPMs :: Bool -> FType -> [FPatternMatch] -> [(Bool, String, FType, [FPatternMatch])] -> Bool
hasDifferentSusOrDifferentTypeOrDifferentPMs sus t pms [] = False
hasDifferentSusOrDifferentTypeOrDifferentPMs sus t pms ((sus_, _, t_, pms_) : xs)
  | sus /= sus_ = True
  | t /= t_ = True
  | length pms /= length pms_ = True
  | otherwise = hasDifferentSusOrDifferentTypeOrDifferentPMs sus t pms xs


checkUniqueFunctionDefinitions :: [FFunctionDef] -> ST.StateT UniqueDefS Err UniqueDefS
checkUniqueFunctionDefinitions [] =
    ST.get
checkUniqueFunctionDefinitions (NonSusFFunctionDef t name pms _ : fdefs) = do
    UniqueDefS m <- ST.get
    let registerDefs = if member name m
            then m ! name
            else []
    if hasDifferentSusOrDifferentTypeOrDifferentPMs False t pms registerDefs
        then lift $ fail $ name ++ " declarations don't match"
        else do
            ST.put $ UniqueDefS $ insert name ((False, name, t, pms):registerDefs) m
            checkUniqueFunctionDefinitions fdefs
checkUniqueFunctionDefinitions (SusFFunctionDef (NonSusFFunctionDef t name pms _) : fdefs) = do
    UniqueDefS m <- ST.get
    let registerDefs = if member name m
            then m ! name
            else []
    if hasDifferentSusOrDifferentTypeOrDifferentPMs True t pms registerDefs
        then lift $ fail $ name ++ " declarations don't match"
        else do
            ST.put $ UniqueDefS $ insert name ((True, name, t, pms):registerDefs) m
            checkUniqueFunctionDefinitions fdefs

runCheckUniqueFunctionDefinitions :: [FFunctionDef] -> UniqueDefS -> Err UniqueDefS
runCheckUniqueFunctionDefinitions funDefs state = do
    (_, s) <- runStateT (checkUniqueFunctionDefinitions funDefs) state
    return s

-- runCheckUniqueReferenceDefinitions

checkUniqueDefinitions :: ProgramFormat -> Err ProgramFormat
checkUniqueDefinitions pf@(SITList funDefs refDefs algTypes) = do
    s1 <- runCheckUniqueFunctionDefinitions funDefs $ UniqueDefS empty
    traceD s1 $ return pf