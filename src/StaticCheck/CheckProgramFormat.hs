module StaticCheck.CheckProgramFormat where

import StaticCheck.Format
import StaticCheck.CheckUniqueDefinitions
import StaticCheck.CheckDoubleSus
import StaticCheck.CheckCase
import StaticCheck.CheckTypes
import StaticCheck.CheckMainFunction
import Bnfc.ErrM
import Util.Util

checkProgramFormat :: ProgramFormat -> Err (ProgramFormat, [String])
checkProgramFormat pf = do
    checkDoubleSus pf
    checkCase pf
    checkUniqueDefinitions pf
    (_, warnings) <- checkTypes pf
    checkMainFunction pf
    return (pf, warnings)