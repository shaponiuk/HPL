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
extractTypes name t@(FTypeT (Just pos) _) (_:_) = fail $ "type " ++ show t ++ " of function " ++ name ++ " shows insufficient number of arguments " ++ show pos
extractTypes _ t [] = return ([], t)
extractTypes _ (FTypeB Nothing _ _) _ = undefined
extractTypes _ (FTypeT Nothing _) _ = undefined

registerArgs :: [FType] -> [FPatternMatch] -> TCE -> Err TCE
registerArgs [] [] tce = return tce
registerArgs (t:types) (pm:pms) tce = do
    tce2 <- registerArg t pm tce
    registerArgs types pms tce2
registerArgs (FTypeB (Just loc) _ _:_) [] _ =
    fail $ "more types declared than given at " ++ show loc
registerArgs (FTypeT (Just loc) _:_) [] _ =
    fail $ "more types declared than given at " ++ show loc
registerArgs (FunFType (Just loc) _ _:_) [] _ =
    fail $ "more types declared than given at " ++ show loc
registerArgs [] (FPatternMatchI (Just loc) _:_) _ =
    fail $ "not enough types for pattern match at " ++ show loc
registerArgs [] (FPatternMatchB (Just loc) _:_) _ =
    fail $ "not enough types for pattern match at " ++ show loc
registerArgs [] (FPatternMatchC (Just loc) _ _:_) _ =
    fail $ "not enough types for pattern match at " ++ show loc
registerArgs [] (FPatternMatchT (Just loc) _:_) _ =
    fail $ "not enough types for pattern match at " ++ show loc
registerArgs (FTypeB Nothing _ _:_) [] _ = undefined
registerArgs (FTypeT Nothing _:_) [] _ = undefined
registerArgs (FunFType Nothing _ _:_) [] _ = undefined
registerArgs [] (FPatternMatchI Nothing _:_) _ = undefined
registerArgs [] (FPatternMatchT Nothing _:_) _ = undefined
registerArgs [] (FPatternMatchC Nothing _ _:_) _ = undefined
registerArgs [] (FPatternMatchB Nothing _:_) _ = undefined


checkExistingTypes :: [FType] -> TCE -> Err ()
checkExistingTypes [] _ = return ()
checkExistingTypes (x:xs) tce = do
    checkExistingType x tce
    checkExistingTypes xs tce

checkExistingType :: FType -> TCE -> Err ()
checkExistingType (FTypeB _ "Ref" [x]) tce = checkExistingType x tce
checkExistingType (FTypeB _ "Ref" _) _ = undefined
checkExistingType (FTypeB _ "Semaphore" []) _ = return ()
checkExistingType (FTypeB _ "Semaphore" _) _ = undefined
checkExistingType (FTypeB _ "Int" []) _ = return ()
checkExistingType (FTypeB _ "Int" _) _ = undefined
checkExistingType (FTypeB _ "String" []) _ = return ()
checkExistingType (FTypeB _ "String" _) _ = undefined
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
checkExistingType (FTypeB Nothing _ _) _ = undefined

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
checkConstructorExistence _ _ _ (FAlgTypeVal (Just pos) _ FunFType{}:_) = 
    fail $ "function type not allowed in algebraic type constructor type " ++ show pos
checkConstructorExistence _ _ _ _ = undefined

mapType :: FType -> Map String FType -> FType
mapType (FTypeT pos types) m = FTypeT pos $ Prelude.map (`mapType` m) types
mapType (FTypeB pos typeName typeArgs) m =
    if member typeName m
        then m ! typeName
        else FTypeB pos typeName $ Prelude.map (`mapType` m) typeArgs
mapType (FunFType pos t1 t2) m =
    FunFType pos (mapType t1 m) (mapType t2 m)

getCorrectedConstructor :: FAlgTypeVal -> FAlgType -> [FType] -> Err FAlgTypeVal
getCorrectedConstructor atv@(FAlgTypeVal pos cName ct) at@(FAlgType _ tName tArgs atvs) types = do
    let m = fromList $ dList tArgs types
    return $ FAlgTypeVal pos cName $ mapType ct m
    
checkMatchingType :: FType -> FPatternMatch -> TCE -> Err ()
checkMatchingType (FTypeB _ "Int" []) FPatternMatchB{} _ = return ()
checkMatchingType (FTypeB _ "Int" []) FPatternMatchI{} _ = return ()
checkMatchingType (FTypeB _ "String" []) (FPatternMatchB _ _) _ = return ()
checkMatchingType (FTypeB _ atName atArgs) (FPatternMatchB _ _) _ = return ()
checkMatchingType (FTypeB _ atName atArgs) (FPatternMatchC (Just pos) (FPatternMatchB _ cName) cArgs) tce =
    if atName /= cName
        then fail $ "wrong constructor name at " ++ show pos
        else checkMatchingTypes atArgs cArgs tce
checkMatchingType (FTypeB _ atName atArgs) (FPatternMatchC (Just pos) _ _) _ =
    fail $ "wrong constructor syntax at " ++ show pos
checkMatchingType _ (FPatternMatchC Nothing _ _) _ = undefined
checkMatchingType t@FTypeB{} (FPatternMatchI (Just pos) _) _ =
    fail $ "int value at " ++ show pos ++ " is not of the type " ++ show t
checkMatchingType _ (FPatternMatchI Nothing _) _ = undefined
checkMatchingType t@FTypeB{} (FPatternMatchT (Just pos) _) _ =
    fail $ "tuple value at " ++ show pos ++ " is not of the type " ++ show t
checkMatchingType _ (FPatternMatchT Nothing _) _ = undefined
checkMatchingType FunFType{} FPatternMatchB{} _ = return ()
checkMatchingType FunFType{} (FPatternMatchC (Just loc) _ _) _ =
    fail $ "constructor at " ++ show loc ++ " is not of the function type"
checkMatchingType FunFType{} (FPatternMatchI (Just loc) _) _ =
    fail $ "int value at " ++ show loc ++ " is not of the function type"
checkMatchingType FunFType{} (FPatternMatchT (Just loc) _) _ =
    fail $ "tuple value at " ++ show loc ++ " is not of the function type"
checkMatchingType (FTypeT _ ts) (FPatternMatchT (Just loc) ts_) tce =
    if length ts /= length ts_
        then fail $ "tuple length at " ++ show loc ++ " doesn't match the tuple length in its type declaration"
        else checkMatchingTypes ts ts_ tce
checkMatchingType FTypeT{} FPatternMatchB{} _ = return ()
checkMatchingType FTypeT{} (FPatternMatchI (Just loc) _) _ =
    fail $ "int value at " ++ show loc ++ " is not of the tuple type"
checkMatchingType FTypeT{} (FPatternMatchC (Just loc) _ _) _ =
    fail $ "type constructor at " ++ show loc ++ " is not of the tuple type"

checkMatchingTypes :: [FType] -> [FPatternMatch] -> TCE -> Err ()
checkMatchingTypes [] [] _ = return ()
checkMatchingTypes (x:xs) (y:ys) tce = do
    checkMatchingType x y tce
    checkMatchingTypes xs ys tce
checkMatchingTypes (FTypeB (Just pos) _ _:_) [] _ = 
    fail $ "too many type arguments at " ++ show pos
checkMatchingTypes (FTypeB Nothing _ _:_) _ _ = undefined
checkMatchingTypes (FunFType (Just pos) _ _:_) [] _ =
    fail $ "too many type arguments at " ++ show pos
checkMatchingTypes (FunFType Nothing _ _:_) [] _ = undefined
checkMatchingTypes (FTypeT (Just pos) _:_) [] _ =
    fail $ "too many type arguments at " ++ show pos
checkMatchingTypes (FTypeT Nothing _:_) [] _ = undefined
checkMatchingTypes [] (FPatternMatchB (Just pos) _:_) _ =
    fail $ "too many variables for the types at " ++ show pos
checkMatchingTypes _ (FPatternMatchB Nothing _:_) _ = undefined
checkMatchingTypes [] (FPatternMatchI (Just pos) _:_) _ =
    fail $ "too many variables for the types at " ++ show pos
checkMatchingTypes _ (FPatternMatchI Nothing _:_) _ = undefined
checkMatchingTypes [] (FPatternMatchC (Just pos) _ _:_) _ =
    fail $ "too many variables for the types at " ++ show pos
checkMatchingTypes _ (FPatternMatchC Nothing _ _:_) _ = undefined
checkMatchingTypes _ (FPatternMatchT (Just pos) _:_) _ =
    fail $ "too many variables for the types at " ++ show pos
checkMatchingTypes _ (FPatternMatchT Nothing _:_) _ = undefined

checkMatchingConstructors :: FAlgTypeVal -> FPatternMatch -> TCE -> Err ()
checkMatchingConstructors (FAlgTypeVal _ cName (FTypeT _ tArgs)) (FPatternMatchC (Just pos) _ cArgs) tce =
    if length tArgs /= length cArgs 
        then fail $ "number of type arguments at " ++ show pos ++ " doesn't match the correct number of arguments for constructor " ++ cName
        else checkMatchingTypes tArgs cArgs tce
checkMatchingConstructors _ _ _ = undefined

registerConstructor :: FAlgTypeVal -> FPatternMatch -> TCE -> Err TCE
registerConstructor (FAlgTypeVal _ _ (FTypeT _ tArgs)) (FPatternMatchC _ _ cArgs) tce =
    registerArgs tArgs cArgs tce
registerConstructor _ _ _ = undefined

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
registerArg _ (FPatternMatchI (Just pos) _) _ =
    fail $ "int value at " ++ show pos ++ " is not of the type Int ()"
registerArg _ (FPatternMatchI Nothing _) _ = undefined
registerArg _ (FPatternMatchT (Just pos) _) _ =
    fail $ "tuple value at " ++ show pos ++ " is not of the tuple type"
registerArg _ (FPatternMatchT Nothing _) _ = undefined
registerArg _ (FPatternMatchC (Just pos) _ _) _ =
    fail $ "algebraic type at " ++ show pos ++ " is not of the algebraic type"
registerArg _ (FPatternMatchC Nothing _ _) _ = undefined

registerAssignments :: String -> [FAssignment] -> TCE -> Err TCE
registerAssignments _ [] tce = return tce
registerAssignments funName (FAssignmentB (Just pos) t pm vs:assignments) tce = do
    checkFunctionBody ("being an assignment " ++ show pos ++ " " ++ show pm ++ " in function " ++ funName) t vs tce
    tce2 <- registerArg t pm tce
    registerAssignments funName assignments tce2
registerAssignments _ (FAssignmentB Nothing _ _ _:_) _ = undefined 
registerAssignments funName (FRefAssignment (Just pos) (FRefDef refDefPos t name vs):assignments) tce = do
    checkFunctionBody ("being a ref assignment " ++ show pos ++ " " ++ name ++ " in function " ++ funName) t vs tce
    tce2 <- registerArg t (FPatternMatchB refDefPos name) tce
    registerAssignments funName assignments tce2
registerAssignments funName (FRefAssignment Nothing _:_) tce = undefined

checkFunctionApplicationTypeInt :: String -> FType -> String -> Maybe (Int, Int) -> FType -> [FValueStatement] -> TCE -> Err ()
checkFunctionApplicationTypeInt funName t1 name posM (FunFType _ t21 t22) (vs:vss) tce = do
    checkFunctionBody funName t21 vs tce
    checkFunctionApplicationTypeInt funName t1 name posM t22 vss tce
checkFunctionApplicationTypeInt funName t1 name (Just pos) t2 [] _ =
    if t1 == t2
        then return ()
        else fail $ "wrong types in function apllication of " ++ name ++ " in " ++ funName ++ " " ++ show pos
checkFunctionApplicationTypeInt _ _ _ _ _ _ _ = undefined

checkFunctionApplicationType :: String -> FType -> String -> Maybe (Int, Int) -> [FValueStatement] -> TCE -> Err ()
checkFunctionApplicationType funName (FTypeT _ []) "print" _ [_] tce = return ()
checkFunctionApplicationType funName (FTypeT _ []) "yield" _ [] _ = return ()
checkFunctionApplicationType funName (FTypeT _ []) "v" _ [x] tce =
    checkFunctionBody funName (FTypeB Nothing "Semaphore" []) x tce
checkFunctionApplicationType funName (FTypeT _ []) "p" _ [x] tce =
    checkFunctionBody funName (FTypeB Nothing "Semaphore" []) x tce
checkFunctionApplicationType funName t "get" _ [x] tce =
    checkFunctionBody funName (FTypeB Nothing "Ref" [t]) x tce
checkFunctionApplicationType funName (FTypeT _ []) "set" (Just pos) [FAValueStatement _ (FFunApplicationB _ name []), value] tce@(TCE tm atm) =
    if member name tm
        then
            case tm ! name of
                FTypeB _ "Ref" [t] ->
                    checkFunctionBody funName t value tce
                _ -> fail $ "the first argument of set at " ++ show pos ++ " is not a ref"
        else fail $ "use of undeclared function name " ++ name ++ " in function " ++ funName ++ " " ++ show pos
checkFunctionApplicationType funName (FTypeB _ "Ref" [FTypeB _ "Semaphore" []]) "make_semaphore" _ [] _ = return ()
checkFunctionApplicationType funName t name (Just pos) args tce@(TCE tm atm) =
    if member name tm
        then checkFunctionApplicationTypeInt funName t name (Just pos) (tm ! name) args tce
        else fail $ "use of undeclared function name " ++ name ++ " in function " ++ funName ++ " " ++ show pos
checkFunctionApplicationType _ _ _ Nothing _ _ = undefined

checkTupleFunctionBody :: String -> [FType] -> [FValueStatement] -> TCE -> Err ()
checkTupleFunctionBody _ [] [] tce = return ()
checkTupleFunctionBody funName (t:types) (vs:vss) tce = do
    checkFunctionBody funName t vs tce
    checkTupleFunctionBody funName types vss tce
checkTupleFunctionBody _ _ _ _ = undefined

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
checkFunctionBody funName t@(FunFType _ t1 t2) vs@(FIValueStatement (Just posVS) _) _ =
    fail $ show vs ++ " " ++ show posVS ++ " is not of the type " ++ show t
checkFunctionBody funName FTypeB{} (FTValueStatement (Just pos) _) _ =
    fail $ "tuple at " ++ show pos ++ " in function " ++ funName ++ " has a non tuple type"
checkFunctionBody _ _ (FAValueStatement _ FFunApplicationR{}) _ = undefined
checkFunctionBody funName t (FIValueStatement (Just loc) i) _ =
    fail $ "int value " ++ show i ++ " at " ++ show loc ++ " is not of the type Int ()"
checkFunctionBody _ _ (FIValueStatement Nothing _) _ = undefined
checkFunctionBody funName t (FLitStrValueStatement (Just loc) i) _ =
    fail $ "string value " ++ show i ++ " at " ++ show loc ++ " is not of the type String ()"
checkFunctionBody _ _ (FLitStrValueStatement Nothing _) _ = undefined
checkFunctionBody funName t@(FTypeB _ "Int" []) (FExpr (Just loc) (FEAdd vs1 vs2)) tce = do
    checkFunctionBody funName t vs1 tce
    checkFunctionBody funName t vs2 tce
checkFunctionBody funName t@(FTypeB _ "Int" []) (FExpr (Just loc) (FESub vs1 vs2)) tce = do
    checkFunctionBody funName t vs1 tce
    checkFunctionBody funName t vs2 tce
checkFunctionBody funName t@(FTypeB _ "Int" []) (FExpr (Just loc) (FEMul vs1 vs2)) tce = do
    checkFunctionBody funName t vs1 tce
    checkFunctionBody funName t vs2 tce
checkFunctionBody funName t@(FTypeB _ "Int" []) (FExpr (Just loc) (FEDiv vs1 vs2)) tce = do
    checkFunctionBody funName t vs1 tce
    checkFunctionBody funName t vs2 tce
checkFunctionBody funName t@(FTypeB _ "Int" []) (FExpr (Just loc) (FEMod vs1 vs2)) tce = do
    checkFunctionBody funName t vs1 tce
    checkFunctionBody funName t vs2 tce
checkFunctionBody funName t@(FTypeB _ "Int" []) (FExpr (Just loc) (FEL vs1 vs2)) tce = do
    checkFunctionBody funName t vs1 tce
    checkFunctionBody funName t vs2 tce
checkFunctionBody funName t@(FTypeB _ "Int" []) (FExpr (Just loc) (FELQ vs1 vs2)) tce = do
    checkFunctionBody funName t vs1 tce
    checkFunctionBody funName t vs2 tce
checkFunctionBody funName t@(FTypeB _ "Int" []) (FExpr (Just loc) (FEG vs1 vs2)) tce = do
    checkFunctionBody funName t vs1 tce
    checkFunctionBody funName t vs2 tce
checkFunctionBody funName t@(FTypeB _ "Int" []) (FExpr (Just loc) (FEGQ vs1 vs2)) tce = do
    checkFunctionBody funName t vs1 tce
    checkFunctionBody funName t vs2 tce
checkFunctionBody funName t@(FTypeB _ "Int" []) (FExpr (Just loc) (FEEQ vs1 vs2)) tce = do
    checkFunctionBody funName t vs1 tce
    checkFunctionBody funName t vs2 tce
checkFunctionBody funName t@(FTypeB _ "Int" []) (FExpr (Just loc) (FENE vs1 vs2)) tce = do
    checkFunctionBody funName t vs1 tce
    checkFunctionBody funName t vs2 tce
checkFunctionBody funName _ (FExpr (Just loc) _) tce =
    fail $ "expresion at " ++ show loc ++ " is not of the type Int ()"
checkFunctionBody _ _ (FExpr Nothing _) _ = undefined
checkFunctionBody _ _ FRefAddr{} _ = undefined
checkFunctionBody _ _ FSusValueStatement{} _ = undefined
checkFunctionBody _ _ FSuspendedValue{} _ = undefined
checkFunctionBody _ _ FSemaphore{} _ = undefined
checkFunctionBody _ _ FNTValueStatement{} _ = undefined
checkFunctionBody funName _ (FTValueStatement (Just pos) _) _ =
    fail $ "tuple at " ++ show pos ++ " in function " ++ funName ++ " is not of the tuple type"
checkFunctionBody _ _ (FTValueStatement Nothing _) _ = undefined
checkFunctionBody funName _ (FCValueStatement (Just pos) _ _) _ =
    fail $ "algebraic type constructor at " ++ show pos ++ " in function " ++ funName ++ " is not of the algebraic type type"
checkFunctionBody _ _ (FCValueStatement Nothing _ _) _ = undefined

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