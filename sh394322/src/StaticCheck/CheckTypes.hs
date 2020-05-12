module StaticCheck.CheckTypes where

import StaticCheck.Format
import Bnfc.ErrM
import Util.Util
import Data.Map

data TCE = TCE (Map String FType) (Map String FAlgType)
    deriving (Show)

intType :: FType
intType = FTypeB "Int" []

strType :: FType
strType = FTypeB "String" []

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

checkExistingType :: FType -> TCE -> Err ()
checkExistingType (FTypeB "Int" []) tce = return ()
checkExistingType (FunFType t1 t2) tce = do
    checkExistingType t1 tce
    checkExistingType t2 tce

registerArg :: FType -> FPatternMatch -> TCE -> Err TCE
registerArg t@(FTypeB _ _) (FPatternMatchB x) tce@(TCE tm atm) = do
    checkExistingType t tce
    return $ TCE (insert x t tm) atm
registerArg (FTypeT ts) (FPatternMatchT pms) tce = registerArgs ts pms tce
registerArg t@(FunFType t1 t2) (FPatternMatchB x) tce@(TCE tm atm) = do
    checkExistingType t1 tce
    checkExistingType t2 tce
    return $ TCE (insert x t tm) atm
registerArg t pm tce = traceD (show t ++ show pm) undefined

registerAssignments :: String -> [FAssignment] -> TCE -> Err TCE
registerAssignments _ [] tce = return tce
registerAssignments funName (FAssignmentB t pm vs:assignments) tce = do
    checkFunctionBody ("being an assignment " ++ show pm ++ " in function " ++ funName) t vs tce
    tce2 <- registerArg t pm tce
    registerAssignments funName assignments tce2

checkIntExpressionInt :: String -> FValueStatement -> FValueStatement -> TCE -> Err ()
checkIntExpressionInt funName vs1 vs2 tce = do
    checkFunctionBody funName intType vs1 tce
    checkFunctionBody funName intType vs2 tce
    return ()

checkIntExpression :: String -> FValueStatementExpr -> TCE -> Err ()
checkIntExpression funName (FEEQ vs1 vs2) = checkIntExpressionInt funName vs1 vs2
checkIntExpression funName (FEMul vs1 vs2) = checkIntExpressionInt funName vs1 vs2
checkIntExpression funName (FESub vs1 vs2) = checkIntExpressionInt funName vs1 vs2
checkIntExpression _ expr = traceD expr undefined

checkFunctionApplicationTypeInt :: String -> FType -> String -> FType -> [FValueStatement] -> TCE -> Err ()
checkFunctionApplicationTypeInt funName t1 name (FunFType t21 t22) (vs:vss) tce = do
    checkFunctionBody funName t21 vs tce
    checkFunctionApplicationTypeInt funName t1 name t22 vss tce
checkFunctionApplicationTypeInt funName t1 name t2 [] _ =
    if t1 == t2
        then return ()
        else fail $ "wrong types in function apllication of " ++ name ++ " in " ++ funName
checkFunctionApplicationTypeInt _ t1 _ t2 vss _ = traceD (show t1 ++ show t2 ++ show vss) undefined

checkIntOrString :: FValueStatement -> TCE -> Bool
checkIntOrString vs tce =
    traceD vs $ case checkFunctionBody "" intType vs tce of
        Bad _ -> case checkFunctionBody "" strType vs tce of
            Bad _ -> False
            Ok _ -> True
        Ok _ -> True

checkFunctionApplicationType :: String -> FType -> String -> [FValueStatement] -> TCE -> Err ()
checkFunctionApplicationType funName (FTypeT []) "print" [x] tce =
    if checkIntOrString x tce
        then return ()
        else fail $ "argument of function print is not Int () or String () in function " ++ funName
checkFunctionApplicationType funName t name args tce@(TCE tm atm) =
    if member name tm
        then checkFunctionApplicationTypeInt funName t name (tm ! name) args tce
        else fail $ "use of undeclared function name " ++ name ++ " in function " ++ funName

checkFunctionBody :: String -> FType -> FValueStatement -> TCE -> Err ()
checkFunctionBody funName (FunFType t1 t2) (FFValueStatement name vs) tce = do
    tce2 <- registerArg t1 (FPatternMatchB name) tce
    checkFunctionBody funName t2 vs tce2
checkFunctionBody funName t FFValueStatement{} _ = fail $ "function " ++ funName ++ " has a non function return type " ++ show t ++ " while it returns a function"
checkFunctionBody funName t (FForceValueStatement assignments vs) tce = do
    tce2 <- registerAssignments funName assignments tce
    checkFunctionBody funName t vs tce2
checkFunctionBody funName t (FAValueStatement (FFunApplicationB name args)) tce =
    checkFunctionApplicationType funName t name args tce
checkFunctionBody funName (FTypeB "Int" []) (FExpr expr) tce = checkIntExpression funName expr tce
checkFunctionBody funName t (FIfValueStatement ifvs vs1 vs2) tce = do
    checkFunctionBody funName (FTypeB "Int" []) ifvs tce
    checkFunctionBody funName t vs1 tce
    checkFunctionBody funName t vs2 tce
    return ()
checkFunctionBody _ intType (FIValueStatement _) _ = return ()
checkFunctionBody funName strType (FIValueStatement _) _ = fail $ "function " ++ funName ++ " has an Int () value while expecting String ()"
checkFunctionBody funName intType (FLitStrValueStatement _) _ = fail $ "function " ++ funName ++ " has a String () value while expecting Int ()"
checkFunctionBody _ strType (FLitStrValueStatement _) _ = return ()
checkFunctionBody _ t vs _ = traceD (show t ++ show vs) undefined

checkFunctionDefs :: [FFunctionDef] -> TCE -> Err ()
checkFunctionDefs [] _ = return ()
checkFunctionDefs (NonSusFFunctionDef t name pms vs:functionDefs) tce = do
    (argTypes, returnType) <- extractTypes name t pms
    tce2 <- registerArgs argTypes pms tce
    checkFunctionBody name returnType vs tce2
    checkFunctionDefs functionDefs tce
checkFunctionDefs (SusFFunctionDef fd:functionDefs) tce = checkFunctionDefs (fd:functionDefs) tce

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