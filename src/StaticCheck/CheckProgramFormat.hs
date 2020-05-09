module StaticCheck.CheckProgramFormat where

import StaticCheck.Format
import StaticCheck.CheckUniqueDefinitions
import Bnfc.ErrM

checkProgramFormat :: ProgramFormat -> Err ProgramFormat
checkProgramFormat pf = do
    -- check double sus
    -- check upper / lower case
    checkUniqueDefinitions pf
    return pf