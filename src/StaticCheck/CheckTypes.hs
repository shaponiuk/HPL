module StaticCheck.CheckTypes where

import StaticCheck.Format
import Bnfc.ErrM
import Util.Util
import Data.Map

data TCE = TCE (Map String FType) (Map String FAlgType)
    deriving (Show)

registerAlgTypes :: [FAlgType] -> TCE -> TCE
registerAlgTypes [] tce = tce
registerAlgTypes (at@(FAlgType name _ _):algTypes) (TCE mt mat) =
    registerAlgTypes algTypes $ TCE mt (insert name at mat)

registerFunctionDefs :: [FFunctionDef] -> TCE -> TCE
registerFunctionDefs [] tce = tce
registerFunctionDefs (NonSusFFunctionDef t name _ _:functionDefs) (TCE mt mat) =
    registerFunctionDefs functionDefs $ TCE (insert name t mt) mat
registerFunctionDefs (SusFFunctionDef fd:functionDefs) tce =
    registerFunctionDefs (fd:functionDefs) tce

registerRefDefs :: [FRefDef] -> TCE -> TCE
registerRefDefs [] tce = tce
registerRefDefs (FRefDef t name _:refDefs) (TCE mt mat) =
    registerRefDefs refDefs $ TCE (insert name (FTypeB "Ref" [t]) mt) mat

checkFunctionDefs :: [FFunctionDef] -> TCE -> Err ()
checkFunctionDefs [] _ = return ()
checkFunctionDefs (NonSusFFunctionDef t name pms vs) tce = do
    (argTypes, returnType) <- exctractTypes t pms
    tce2 <- registerArgs argTypes pms tce

checkRefDefs :: [FRefDef] -> TCE -> Err ()
checkRefDefs _ _ = return ()

checkTypes :: ProgramFormat -> Err ProgramFormat
checkTypes pf@(SITList functionDefs refDefs algTypes) = do
    let tce0 = registerAlgTypes algTypes $ TCE empty empty
    let tce1 = registerFunctionDefs functionDefs tce0
    let tce2 = registerRefDefs refDefs tce1
    checkFunctionDefs functionDefs tce2
    checkRefDefs refDefs tce2
    traceD tce2 $ return pf