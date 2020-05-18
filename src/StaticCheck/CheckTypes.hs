module StaticCheck.CheckTypes where

import StaticCheck.Format
import Bnfc.ErrM
import Util.Util
import Data.Map

data TCE = TCE (Map String FType) (Map String FAlgType)
    deriving (Show)

newTCE :: TCE
newTCE = TCE empty $ insert "List" 
    (FAlgType Nothing "List" ["a"] 
        [
            FAlgTypeVal Nothing "ListC" $ FTypeT Nothing [FTypeB Nothing "a" [], FTypeB Nothing "List" [FTypeB Nothing "a" []]],
            FAlgTypeVal Nothing "EmptyListC" $ FTypeT Nothing []
        ]
    ) empty

registerAlgTypes :: [FAlgType] -> TCE -> TCE
registerAlgTypes [] tce = tce
registerAlgTypes (at@(FAlgType _ name _ _):algTypes) (TCE mt mat) =
    registerAlgTypes algTypes $ TCE mt (insert name at mat)

registerFunctionDefs :: [FFunctionDef] -> TCE -> TCE
registerFunctionDefs [] tce = tce
registerFunctionDefs (NonSusFFunctionDef _ t name _ _:functionDefs) (TCE mt mat) =
    registerFunctionDefs functionDefs $ TCE (insert name t mt) mat
registerFunctionDefs (SusFFunctionDef fd:functionDefs) tce =
    registerFunctionDefs (fd:functionDefs) tce

registerRefDefs :: [FRefDef] -> TCE -> TCE
registerRefDefs [] tce = tce
registerRefDefs (FRefDef _ t name _:refDefs) (TCE mt mat) =
    registerRefDefs refDefs $ TCE (insert name (FTypeB Nothing "Ref" [t]) mt) mat

extractTypes :: String -> FType -> [FPatternMatch] -> Err ([FType], FType)
extractTypes name (FunFType _ t1 t2) (_:xs) = do
    (argTypes, resultType) <- extractTypes name t2 xs
    return (t1:argTypes, resultType)
extractTypes name t@(FTypeB (Just pos) _ _) (_:_) = fail $ "type " ++ show t ++ " of function " ++ name ++ " shows insufficient number of arguments " ++ show pos
extractTypes _ t [] = return ([], t)

registerArgs :: [FType] -> [FPatternMatch] -> TCE -> Err TCE
registerArgs [] [] tce = return tce
registerArgs (t:types) (pm:pms) tce = do
    tce2 <- registerArg t pm tce
    registerArgs types pms tce2

checkExistingTypes :: [FType] -> TCE -> Err ()
checkExistingTypes [] _ = return ()
checkExistingTypes (x:xs) tce = do
    checkExistingType x tce
    checkExistingTypes xs tce

checkExistingType :: FType -> TCE -> Err ()
checkExistingType (FTypeB _ "Int" []) tce = return ()
checkExistingType (FTypeB _ "String" []) tce = return ()
checkExistingType (FunFType _ t1 t2) tce = do
    checkExistingType t1 tce
    checkExistingType t2 tce
checkExistingType (FTypeT _ []) _ = return ()
checkExistingType (FTypeT _ (x:xs)) tce = do
    checkExistingType x tce
    checkExistingType (FTypeT Nothing xs) tce
checkExistingType (FTypeB (Just pos) name args) tce@(TCE _ atm) =
    if member name atm
        then do
            let (FAlgType _ name_ argTypes _) = atm ! name
            if length argTypes == length args
                then
                    checkExistingTypes args tce
                else
                    fail $ "received wrong number of arguments for type " ++ name ++ " " ++ show pos
        else 
            fail $ "type " ++ name ++ " doesn't exist " ++ show pos

checkConstructorExistence :: String -> String -> Int -> [FAlgTypeVal] -> Err FAlgTypeVal
checkConstructorExistence typeName cName _ [] = fail $ "constructor " ++ cName ++ " is not from the type " ++ typeName
checkConstructorExistence typeName cName argCount (atv@(FAlgTypeVal _ cName_ FTypeB{}):atvs) =
    if cName == cName_ && argCount == 1
        then return atv
        else checkConstructorExistence typeName cName argCount atvs
checkConstructorExistence typeName cName argCount (atv@(FAlgTypeVal _ cName_ (FTypeT _ types)):atvs) =
    if cName == cName_ && argCount == length types
        then return atv
        else checkConstructorExistence typeName cName argCount atvs

mapType :: FType -> Map String FType -> FType
mapType (FTypeT pos types) m = FTypeT pos $ Prelude.map (`mapType` m) types
mapType (FTypeB pos typeName typeArgs) m =
    if member typeName m
        then m ! typeName
        else FTypeB pos typeName $ Prelude.map (`mapType` m) typeArgs
mapType t m = traceD t undefined

getCorrectedConstructor :: FAlgTypeVal -> FAlgType -> [FType] -> Err FAlgTypeVal
getCorrectedConstructor atv@(FAlgTypeVal pos cName ct) at@(FAlgType _ tName tArgs atvs) types = do
    let m = fromList $ dList tArgs types
    return $ FAlgTypeVal pos cName $ mapType ct m
    
checkMatchingType :: FType -> FPatternMatch -> TCE -> Err ()
checkMatchingType (FTypeB _ "Int" []) (FPatternMatchB _ _) _ = return ()
checkMatchingType (FTypeB _ "String" []) (FPatternMatchB _ _) _ = return ()
checkMatchingType (FTypeB _ atName atArgs) (FPatternMatchB _ _) _ = return ()
checkMatchingType t pm tce = traceD (show t ++ show pm) undefined

checkMatchingTypes :: [FType] -> [FPatternMatch] -> TCE -> Err ()
checkMatchingTypes [] [] _ = return ()
checkMatchingTypes (x:xs) (y:ys) tce = do
    checkMatchingType x y tce
    checkMatchingTypes xs ys tce

checkMatchingConstructors :: FAlgTypeVal -> FPatternMatch -> TCE -> Err ()
checkMatchingConstructors (FAlgTypeVal _ cName (FTypeT _ tArgs)) (FPatternMatchC _ _ cArgs) tce =
    checkMatchingTypes tArgs cArgs tce
checkMatchingConstructors atv pm tce = traceD (show atv ++ show pm) undefined

registerConstructor :: FAlgTypeVal -> FPatternMatch -> TCE -> Err TCE
registerConstructor (FAlgTypeVal _ _ (FTypeT _ tArgs)) (FPatternMatchC _ _ cArgs) tce =
    registerArgs tArgs cArgs tce
registerConstructor atv pm (TCE tm atm) = traceD (show atv ++ show pm) undefined

registerArg :: FType -> FPatternMatch -> TCE -> Err TCE
registerArg t@FTypeB{} (FPatternMatchB _ x) tce@(TCE tm atm) = do
    checkExistingType t tce
    return $ TCE (insert x t tm) atm
registerArg (FTypeT _ ts) (FPatternMatchT _ pms) tce = registerArgs ts pms tce
registerArg t@(FunFType _ t1 t2) (FPatternMatchB _ x) tce@(TCE tm atm) = do
    checkExistingType t1 tce
    checkExistingType t2 tce
    return $ TCE (insert x t tm) atm
registerArg t@(FTypeT _ _) (FPatternMatchB _ x) tce@(TCE tm atm) = do
    checkExistingType t tce
    return $ TCE (insert x t tm) atm
registerArg t@(FTypeB _ name args) pmc@(FPatternMatchC _ (FPatternMatchB _ cName) cArgs) tce@(TCE tm atm) = do
    let argCount = length cArgs
    checkExistingType t tce
    let at@(FAlgType _ _ argTypes atvs) = atm ! name
    atm <- checkConstructorExistence name cName argCount atvs
    atm_ <- getCorrectedConstructor atm at args
    checkMatchingConstructors atm_ pmc tce
    registerConstructor atm_ pmc tce
registerArg (FTypeB _ "Int" []) (FPatternMatchI _ _) tce = return tce
registerArg t pm tce = traceD (show t ++ show pm) undefined

registerAssignments :: String -> [FAssignment] -> TCE -> Err TCE
registerAssignments _ [] tce = return tce
registerAssignments funName (FAssignmentB (Just pos) t pm vs:assignments) tce = do
    checkFunctionBody ("being an assignment " ++ show pos ++ " " ++ show pm ++ " in function " ++ funName) t vs tce
    tce2 <- registerArg t pm tce
    registerAssignments funName assignments tce2

checkIntExpressionInt :: String -> FValueStatement -> FValueStatement -> TCE -> Err ()
checkIntExpressionInt funName vs1 vs2 tce = do
    checkFunctionBody funName (FTypeB Nothing "Int" []) vs1 tce
    checkFunctionBody funName (FTypeB Nothing "Int" []) vs2 tce

checkIntExpression :: String -> FValueStatementExpr -> TCE -> Err ()
checkIntExpression funName (FEEQ vs1 vs2) = checkIntExpressionInt funName vs1 vs2
checkIntExpression funName (FEMul vs1 vs2) = checkIntExpressionInt funName vs1 vs2
checkIntExpression funName (FESub vs1 vs2) = checkIntExpressionInt funName vs1 vs2
checkIntExpression funName (FEAdd vs1 vs2) = checkIntExpressionInt funName vs1 vs2
checkIntExpression _ expr = traceD expr undefined

checkFunctionApplicationTypeInt :: String -> FType -> String -> Maybe (Int, Int) -> FType -> [FValueStatement] -> TCE -> Err ()
checkFunctionApplicationTypeInt funName t1 name posM (FunFType _ t21 t22) (vs:vss) tce = do
    checkFunctionBody funName t21 vs tce
    checkFunctionApplicationTypeInt funName t1 name posM t22 vss tce
checkFunctionApplicationTypeInt funName t1 name (Just pos) t2 [] _ =
    if t1 == t2
        then return ()
        else fail $ "wrong types in function apllication of " ++ name ++ " in " ++ funName ++ " " ++ show pos
checkFunctionApplicationTypeInt _ t1 _ _ t2 vss _ = traceD (show t1 ++ show t2 ++ show vss) undefined

checkFunctionApplicationType :: String -> FType -> String -> Maybe (Int, Int) -> [FValueStatement] -> TCE -> Err ()
checkFunctionApplicationType funName (FTypeT _ []) "print" _ [_] tce = return ()
checkFunctionApplicationType funName (FTypeT _ []) "yield" _ [] _ = return ()
checkFunctionApplicationType funName t name (Just pos) args tce@(TCE tm atm) =
    if member name tm
        then checkFunctionApplicationTypeInt funName t name (Just pos) (tm ! name) args tce
        else fail $ "use of undeclared function name " ++ name ++ " in function " ++ funName ++ " " ++ show pos

checkTupleFunctionBody :: String -> [FType] -> [FValueStatement] -> TCE -> Err ()
checkTupleFunctionBody _ [] [] tce = return ()
checkTupleFunctionBody funName (t:types) (vs:vss) tce = do
    checkFunctionBody funName t vs tce
    checkTupleFunctionBody funName types vss tce

checkFunctionBody :: String -> FType -> FValueStatement -> TCE -> Err ()
checkFunctionBody funName (FunFType _ t1 t2) (FFValueStatement posM name vs) tce = do
    tce2 <- registerArg t1 (FPatternMatchB posM name) tce
    checkFunctionBody funName t2 vs tce2
checkFunctionBody funName t FFValueStatement{} _ = fail $ "function " ++ funName ++ " has a non function return type " ++ show t ++ " while it returns a function"
checkFunctionBody funName t (FForceValueStatement _ assignments vs) tce = do
    tce2 <- registerAssignments funName assignments tce
    checkFunctionBody funName t vs tce2
checkFunctionBody funName t (FAValueStatement _ (FFunApplicationB pos name args)) tce =
    checkFunctionApplicationType funName t name pos args tce
checkFunctionBody funName (FTypeB _ "Int" []) (FExpr _ expr) tce = checkIntExpression funName expr tce
checkFunctionBody funName t (FIfValueStatement posM ifvs vs1 vs2) tce = do
    checkFunctionBody funName (FTypeB posM "Int" []) ifvs tce
    checkFunctionBody funName t vs1 tce
    checkFunctionBody funName t vs2 tce
checkFunctionBody _ (FTypeB _ "Int" []) (FIValueStatement _ _) _ = return ()
checkFunctionBody funName (FTypeB _ "String" []) (FIValueStatement (Just posVS) _) _ = 
    fail $ "function " ++ funName ++ " " ++ show posVS ++ " has an Int () value while expecting String ()"
checkFunctionBody funName (FTypeB _ "Int" []) (FLitStrValueStatement (Just posVS) _) _ = 
    fail $ "function " ++ funName ++ " " ++ show posVS ++ " has a String () value while expecting Int ()"
checkFunctionBody _ (FTypeB _ "String" []) (FLitStrValueStatement _ _) _ = return ()
checkFunctionBody funName (FTypeT _ [t]) vs tce = checkFunctionBody funName t vs tce
checkFunctionBody funName (FTypeT _ types) (FTValueStatement _ vss) tce = checkTupleFunctionBody funName types vss tce
checkFunctionBody funName t (FValueStatementB _ assignments vs) tce = do
    tce2 <- registerAssignments funName assignments tce
    checkFunctionBody funName t vs tce2
checkFunctionBody funName t@FTypeB{} (FCValueStatement pos cName [FTValueStatement _ cArgs]) tce =
    checkFunctionBody funName t (FCValueStatement pos cName cArgs) tce
checkFunctionBody funName t@(FTypeB _ atName atArgs) (FCValueStatement pos cName cArgs) tce@(TCE tm atm) = do
    let argCount = length cArgs
    checkExistingType t tce
    let at@(FAlgType _ _ _ atvs) = atm ! atName
    atm <- checkConstructorExistence atName cName argCount atvs
    (FAlgTypeVal _ _ atmArgs) <- getCorrectedConstructor atm at atArgs
    checkFunctionBody funName atmArgs (FTValueStatement pos cArgs) tce
checkFunctionBody _ t vs _ = traceD (show t ++ show vs) undefined

checkFunctionDefs :: [FFunctionDef] -> TCE -> Err ()
checkFunctionDefs [] _ = return ()
checkFunctionDefs (NonSusFFunctionDef _ t name pms vs:functionDefs) tce = do
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
    return pf