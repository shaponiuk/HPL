module StaticCheck.CheckProgramFormat where

import StaticCheck.Format
import StaticCheck.CheckUniqueDefinitions
import StaticCheck.CheckDoubleSus
import StaticCheck.CheckCase
import StaticCheck.CheckTypes
import Bnfc.ErrM
import Util.Util

checkProgramFormat :: ProgramFormat -> Err ProgramFormat
checkProgramFormat pf = do
    checkDoubleSus pf
    checkCase pf
    checkUniqueDefinitions pf
    checkTypes pf
    -- check main function
    traceD pf $ return pf