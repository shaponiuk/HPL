module StaticCheck.CheckProgramFormat where

import StaticCheck.Format
import StaticCheck.CheckUniqueDefinitions
import StaticCheck.CheckDoubleSus
import Bnfc.ErrM
import Util.Util

checkProgramFormat :: ProgramFormat -> Err ProgramFormat
checkProgramFormat pf = do
    checkDoubleSus pf
    -- check upper / lower case
    checkUniqueDefinitions pf
    -- check usage of existing, available values
    -- check types, the variables have correct types
    -- check main function
    traceD pf $ return pf