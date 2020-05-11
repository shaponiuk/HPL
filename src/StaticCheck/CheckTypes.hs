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

extractTypes :: String -> FType -> [FPatternMatch] -> Err ([FType], FType)
extractTypes name (FunFType t1 t2) (_:xs) = do
    (argTypes, resultType) <- extractTypes name t2 xs
    return (t1:argTypes, resultType)
extractTypes name t (_:_) = fail $ "type " ++ show t ++ " of function " ++ name ++ " shows insufficient number of arguments"
extractTypes _ t [] = return ([], t)

registerArgs :: [FType] -> [FPatternMatch] -> TCE -> Err TCE
registerArgs [] [] tce = return tce
registerArgs (t:types) (pm:pms) tce = do
    tce2 <- registerArg t pm tce
    registerArgs types pms tce2

registerArg :: FType -> FPatternMatch -> TCE -> Err TCE
registerArg t@(FTypeB "Int" []) (FPatternMatchB x) (TCE tm atm) =
    return $ TCE (insert x t tm) atm
-- registerArg t pm tce = traceD t undefined

checkFunctionBody :: FValueStatement -> TCE -> Err ()
checkFunctionBody vs tce = traceD vs undefined

checkFunctionDefs :: [FFunctionDef] -> TCE -> Err ()
checkFunctionDefs [] _ = return ()
checkFunctionDefs (NonSusFFunctionDef t name pms vs:functionDefs) tce = do
    (argTypes, returnType) <- extractTypes name t pms
    tce2 <- registerArgs argTypes pms tce
    checkFunctionBody vs tce2
    checkFunctionDefs functionDefs tce

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