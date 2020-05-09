module StaticCheck.CheckUniqueDefinitions where

import StaticCheck.Format
import Bnfc.ErrM
import Util.Util
import Data.Map
import Control.Monad.State
import Control.Monad.Trans.State as ST
import Control.Monad.Trans.Except
import Control.Monad.Trans.Identity

newtype UniqueDefS = UniqueDefS (Map String -> [(Bool, String, FType, [FPatternMatch])])
    deriving (Show)

checkUniqueFunctionDefinitions :: [FFunctionDef] -> ST.StateT UniqueDefS Err UniqueDefS
checkUniqueFunctionDefinitions [] =
    ST.get
checkUniqueFunctionDefinitions (NonSusFFunctionDef t name pms _ : fdefs) = do
    UniqueDefS m <- ST.get
    lift $ fail "test"

runCheckUniqueFunctionDefinitions :: [FFunctionDef] -> UniqueDefS -> Err UniqueDefS
runCheckUniqueFunctionDefinitions funDefs state = do
    (_, s) <- runStateT (checkUniqueFunctionDefinitions funDefs) state
    return s

-- runCheckUniqueReferenceDefinitions

checkUniqueDefinitions :: ProgramFormat -> Err ProgramFormat
checkUniqueDefinitions pf@(SITList funDefs refDefs algTypes) = do
    s1 <- runCheckUniqueFunctionDefinitions funDefs $ UniqueDefS empty
    traceD s1 $ return pf