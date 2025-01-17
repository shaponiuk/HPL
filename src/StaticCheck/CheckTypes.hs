module StaticCheck.CheckTypes where

import StaticCheck.Format
import Bnfc.ErrM
import Util.Util
import Data.Map as M
import Data.Char
import Data.Set as S
import Control.Monad

data TCE = TCE (Map String FType) (Map String FAlgType)
    deriving (Show)

newTCE :: TCE
newTCE = TCE M.empty $ M.insert "List" 
    (FAlgType Nothing "List" ["a"] 
        [
            FAlgTypeVal Nothing "ListC" $ FTypeT Nothing [FTypeB Nothing "a" [], FTypeB Nothing "List" [FTypeB Nothing "a" []]],
            FAlgTypeVal Nothing "EmptyListC" $ FTypeT Nothing []
        ]
    ) $ M.insert "SList"
    (FAlgType Nothing "SList" []
        [
            FAlgTypeVal Nothing "SListC" $ FTypeT Nothing [FTypeB Nothing "Int" [], FTypeB Nothing "SList" []],
            FAlgTypeVal Nothing "SEmptyListC" $ FTypeT Nothing []
        ]
    ) $ M.insert "Maybe"
    (FAlgType Nothing "Maybe" ["a"]
        [
            FAlgTypeVal Nothing "Just" $ FTypeT Nothing [FTypeB Nothing "a" []],
            FAlgTypeVal Nothing "Nothing" $ FTypeT Nothing []
        ]
    ) $ M.insert "Either" 
    (FAlgType Nothing "Either" ["a", "b"]
        [
            FAlgTypeVal Nothing "Left" $ FTypeT Nothing [FTypeB Nothing "a" []],
            FAlgTypeVal Nothing "Right" $ FTypeT Nothing [FTypeB Nothing "b" []]
        ]
    ) M.empty

registerAlgTypes :: [FAlgType] -> TCE -> TCE
registerAlgTypes [] tce = tce
registerAlgTypes (at@(FAlgType _ name _ _):algTypes) (TCE mt mat) =
    registerAlgTypes algTypes $ TCE mt (M.insert name at mat)

registerFunctionDefs :: [FFunctionDef] -> TCE -> TCE
registerFunctionDefs [] tce = tce
registerFunctionDefs (NonSusFFunctionDef _ t name _ _:functionDefs) (TCE mt mat) =
    registerFunctionDefs functionDefs $ TCE (M.insert name t mt) mat
registerFunctionDefs (SusFFunctionDef fd:functionDefs) tce =
    registerFunctionDefs (fd:functionDefs) tce

registerRefDefs :: [FRefDef] -> TCE -> TCE
registerRefDefs [] tce = tce
registerRefDefs (FRefDef _ t name _:refDefs) (TCE mt mat) =
    registerRefDefs refDefs $ TCE (M.insert name (FTypeB Nothing "Ref" [t]) mt) mat

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
checkExistingType (FTypeB _ "SList" []) _ = return ()
checkExistingType (FTypeB _ "SList" _) _ = undefined
checkExistingType (FTypeB _ "List" [x]) tce = checkExistingType x tce
checkExistingType (FTypeB _ "List" _) _ = undefined
checkExistingType (FunFType _ t1 t2) tce = do
    checkExistingType t1 tce
    checkExistingType t2 tce
checkExistingType (FTypeT _ []) _ = return ()
checkExistingType (FTypeT _ (x:xs)) tce = do
    checkExistingType x tce
    checkExistingType (FTypeT Nothing xs) tce
checkExistingType (FTypeB (Just pos) name args) tce@(TCE _ atm) =
    if M.member name atm
        then do
            let (FAlgType _ name_ argTypes _) = atm ! name
            if length argTypes == length args
                then
                    checkExistingTypes args tce
                else
                    fail $ "received wrong number of arguments for type " ++ name ++ " " ++ show pos ++ show argTypes ++ show args
        else 
            fail $ "type " ++ name ++ " doesn't exist " ++ show pos
checkExistingType (FTypeB Nothing _ _) _ = undefined

checkConstructorExistence :: String -> String -> [FAlgTypeVal] -> Err FAlgTypeVal
checkConstructorExistence typeName cName [] = fail $ "constructor " ++ cName ++ " is not from the type " ++ typeName
checkConstructorExistence typeName cName (atv@(FAlgTypeVal _ cName_ FTypeB{}):atvs) =
    if cName == cName_
        then return atv
        else checkConstructorExistence typeName cName atvs
checkConstructorExistence typeName cName (atv@(FAlgTypeVal _ cName_ (FTypeT _ types)):atvs) =
    if cName == cName_
        then return atv
        else checkConstructorExistence typeName cName atvs
checkConstructorExistence _ _ (FAlgTypeVal (Just pos) _ FunFType{}:_) = 
    fail $ "function type not allowed in algebraic type constructor type " ++ show pos
checkConstructorExistence _ _ _ = undefined

mapType :: FType -> Map String FType -> FType
mapType (FTypeT pos types) m = FTypeT pos $ Prelude.map (`mapType` m) types
mapType (FTypeB pos typeName typeArgs) m =
    if M.member typeName m
        then m ! typeName
        else FTypeB pos typeName $ Prelude.map (`mapType` m) typeArgs
mapType (FunFType pos t1 t2) m =
    FunFType pos (mapType t1 m) (mapType t2 m)

getCorrectedConstructor :: FAlgTypeVal -> FAlgType -> [FType] -> Err FAlgTypeVal
getCorrectedConstructor atv@(FAlgTypeVal pos cName ct) at@(FAlgType _ tName tArgs atvs) types = do
    let m = M.fromList $ dList tArgs types
    return $ FAlgTypeVal pos cName $ mapType ct m

atvPMMatchingType :: FType -> FPatternMatch -> TCE -> Maybe FType
atvPMMatchingType (FTypeB _ atName atArgs) (FPatternMatchC (Just pos) (FPatternMatchB _ cName) cArgs) tce@(TCE tm atm) = 
    if M.member atName atm
        then
            let
                (FAlgType _ _ tVars atvs) = atm ! atName  
            in if length tVars == length atArgs
                then
                    let 
                        m = M.fromList $ dList tVars atArgs
                    in case Prelude.filter (\(FAlgTypeVal _ name _) -> name == cName) atvs of
                            (FAlgTypeVal _ _ atvArg:_) ->
                                case checkMatchingType (mapType atvArg m) (FPatternMatchT (Just pos) cArgs) tce of
                                    Bad _ -> Nothing
                                    Ok () -> Just (mapType atvArg m)
                            [] -> Nothing
                else Nothing
        else Nothing
atvPMMatchingType _ _ _ = undefined
    
checkMatchingType :: FType -> FPatternMatch -> TCE -> Err ()
checkMatchingType (FTypeB _ "Int" []) FPatternMatchB{} _ = return ()
checkMatchingType (FTypeB _ "Int" []) FPatternMatchI{} _ = return ()
checkMatchingType (FTypeB _ atName atArgs) (FPatternMatchB _ _) _ = return ()
checkMatchingType t@(FTypeB _ atName atArgs) atvPM@(FPatternMatchC (Just pos) (FPatternMatchB _ cName) cArgs) tce =
    case atvPMMatchingType t atvPM tce of
        Just (FTypeT _ ts) -> checkMatchingTypes ts cArgs tce
        Just t -> checkMatchingTypes [t] cArgs tce
        Nothing -> fail $ "wrong constructor name at " ++ show pos
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
checkMatchingConstructors (FAlgTypeVal _ cName tArg) (FPatternMatchC (Just pos) _ cArgs) tce =
    if 1 /= length cArgs 
        then fail $ "number of type arguments at " ++ show pos ++ " doesn't match the correct number of arguments for constructor " ++ cName
        else checkMatchingTypes [tArg] cArgs tce
checkMatchingConstructors _ (FPatternMatchC Nothing _ _) _ = undefined
checkMatchingConstructors (FAlgTypeVal _ cName _) (FPatternMatchI (Just pos) _) _ = fail $ "wrong pattern match type for constructor " ++ cName ++ " at " ++ show pos
checkMatchingConstructors _ (FPatternMatchI Nothing _) _ = undefined
checkMatchingConstructors (FAlgTypeVal _ cName _) (FPatternMatchT (Just pos) _) _ = fail $ "wrong pattern match type for constructor " ++ cName ++ " at " ++ show pos
checkMatchingConstructors _ (FPatternMatchT Nothing _) _ = undefined
checkMatchingConstructors _ FPatternMatchB{} _ = return ()

registerConstructor :: FAlgTypeVal -> FPatternMatch -> TCE -> Err TCE
registerConstructor (FAlgTypeVal _ _ (FTypeT _ tArgs)) (FPatternMatchC _ _ cArgs) tce =
    registerArgs tArgs cArgs tce
registerConstructor (FAlgTypeVal _ _ tArg) (FPatternMatchC _ _ cArgs) tce =
    registerArgs [tArg] cArgs tce
registerConstructor _ _ _ = undefined

registerArg :: FType -> FPatternMatch -> TCE -> Err TCE
registerArg t@FTypeB{} (FPatternMatchB _ x) tce@(TCE tm atm) = do
    checkExistingType t tce
    return $ TCE (M.insert x t tm) atm
registerArg (FTypeT _ ts) (FPatternMatchT _ pms) tce = registerArgs ts pms tce
registerArg t@(FunFType _ t1 t2) (FPatternMatchB _ x) tce@(TCE tm atm) = do
    checkExistingType t1 tce
    checkExistingType t2 tce
    return $ TCE (M.insert x t tm) atm
registerArg t@(FTypeT _ _) (FPatternMatchB _ x) tce@(TCE tm atm) = do
    checkExistingType t tce
    return $ TCE (M.insert x t tm) atm
registerArg t@(FTypeB _ name args) pmc@(FPatternMatchC _ (FPatternMatchB _ cName) cArgs) tce@(TCE tm atm) = do
    checkExistingType t tce
    let at@(FAlgType _ _ argTypes atvs) = atm ! name
    atm <- checkConstructorExistence name cName atvs
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

checkFunctionApplicationTypeInt :: String -> FType -> String -> (Int, Int) -> FType -> [FValueStatement] -> TCE -> Err ()
checkFunctionApplicationTypeInt funName t1 name posM (FunFType _ t21 t22) (vs:vss) tce = do
    checkFunctionBody funName t21 vs tce
    checkFunctionApplicationTypeInt funName t1 name posM t22 vss tce
checkFunctionApplicationTypeInt funName t1 name pos t2 [] _ =
    if t1 == t2
        then return ()
        else fail $ "wrong types in function apllication of " ++ name ++ " in " ++ funName ++ " " ++ show pos ++ show t1 ++ show t2
checkFunctionApplicationTypeInt funName t1 name pos t2 _ _ = fail $ "too many arguments applied for function " ++ show funName ++ " at " ++ show pos

checkIfTypeIsPrintable :: FType -> TCE -> Err ()
checkIfTypeIsPrintable (FTypeB _ "Int" []) _ = return ()
checkIfTypeIsPrintable (FTypeB _ "Maybe" [t]) tce = checkIfTypeIsPrintable t tce
checkIfTypeIsPrintable (FTypeB _ "Either" [t1, t2]) tce = do
    checkIfTypeIsPrintable t1 tce
    checkIfTypeIsPrintable t2 tce
checkIfTypeIsPrintable (FTypeB _ "List" [t]) tce = checkIfTypeIsPrintable t tce
checkIfTypeIsPrintable (FTypeB _ "SList" []) _ = return ()
checkIfTypeIsPrintable t@(FTypeB (Just loc) _ _) _ = fail $ "type " ++ show t ++ " at " ++ show loc ++ " is not printable"
checkIfTypeIsPrintable t@(FTypeB Nothing _ _) _ = fail $ "type " ++ show t ++ " is not printable"
checkIfTypeIsPrintable (FunFType _ t1 t2) tce = do
    checkIfTypeIsPrintable t1 tce
    checkIfTypeIsPrintable t2 tce
checkIfTypeIsPrintable (FTypeT _ ts) tce = forM_ ts (`checkIfTypeIsPrintable` tce)

checkFunctionApplicationTypePrintable :: FType -> String -> (Int, Int) -> [FValueStatement] -> TCE -> Err ()
checkFunctionApplicationTypePrintable (FunFType _ t1 t2) name loc (vs:vss) tce = do
    checkFunctionBody "print" t1 vs tce
    checkFunctionApplicationTypePrintable t2 name loc vss tce
checkFunctionApplicationTypePrintable FunFType{} _ loc [] _ = fail $ "functions are not printable (at " ++ show loc ++ ")"
checkFunctionApplicationTypePrintable (FTypeB _ "Int" []) _ _ [] _ = return ()
checkFunctionApplicationTypePrintable (FTypeB _ "Maybe" [t]) name loc [] tce = checkFunctionApplicationTypePrintable t name loc [] tce
checkFunctionApplicationTypePrintable (FTypeB _ "Either" [t1, t2]) name loc [] tce = do
    checkFunctionApplicationTypePrintable t1 name loc [] tce
    checkFunctionApplicationTypePrintable t2 name loc [] tce
checkFunctionApplicationTypePrintable (FTypeB _ "List" [t]) name loc [] tce = checkFunctionApplicationTypePrintable t name loc [] tce
checkFunctionApplicationTypePrintable (FTypeB _ "SList" []) _ _ [] _ = return ()
checkFunctionApplicationTypePrintable t@FTypeB{} _ loc [] _ = fail $ "type " ++ show t ++ " at " ++ show loc ++ " is not printable"
checkFunctionApplicationTypePrintable FTypeB{} name loc (_:_) _ = fail $ "too many arguments applied for function " ++ name ++ " at " ++ show loc 
checkFunctionApplicationTypePrintable (FTypeT _ [t]) name loc args tce = checkFunctionApplicationTypePrintable t name loc args tce
checkFunctionApplicationTypePrintable (FTypeT _ ts) name loc args tce = 
    forM_ ts (\x -> checkFunctionApplicationTypePrintable x name loc [] tce)

checkPrintableType :: FValueStatement -> TCE -> Err ()
checkPrintableType FIValueStatement{} _ = return ()
checkPrintableType (FCValueStatement _ "SListC" [h, t]) tce = do
    checkFunctionBody "print" (FTypeB Nothing "Int" []) h tce
    checkFunctionBody "print" (FTypeB Nothing "SList" []) t tce
checkPrintableType (FCValueStatement _ "SEmptyList" []) _ = return ()
checkPrintableType (FCValueStatement _ "Just" [vs]) tce = checkPrintableType vs tce
checkPrintableType (FCValueStatement _ "Nothing" []) _ = return ()
checkPrintableType (FCValueStatement _ "ListC" [h, t]) tce = do
    checkPrintableType h tce
    checkPrintableType t tce
checkPrintableType (FCValueStatement _ "EmptyListC" []) _ = return ()
checkPrintableType (FCValueStatement _ "Left" [vs]) tce = checkPrintableType vs tce
checkPrintableType (FCValueStatement _ "Right" [vs]) tce = checkPrintableType vs tce
checkPrintableType c@(FCValueStatement (Just loc) _ _) _ = fail $ show c ++ " at " ++ show loc ++ " is not printable"
checkPrintableType c@(FCValueStatement Nothing _ _) _ = fail $ show c ++ " is not printable"
checkPrintableType (FAValueStatement (Just loc) (FFunApplicationB _ name args)) tce@(TCE tm atm) =
    if M.member name tm
        then do
            let t = tm ! name
            checkIfTypeIsPrintable t tce
            checkFunctionApplicationTypePrintable t name loc args tce
        else fail $ "name " ++ name ++ " at " ++ show loc ++ " is not registered"
checkPrintableType FAValueStatement{} _ = undefined
checkPrintableType (FValueStatementB _ assignments vs) tce = do
    tce <- registerAssignments "print" assignments tce
    checkPrintableType vs tce
checkPrintableType (FForceValueStatement _ assignments vs) tce = do
    tce <- registerAssignments "print" assignments tce
    checkPrintableType vs tce
checkPrintableType (FIfValueStatement _ condVS vs1 vs2) tce = do
    checkFunctionBody "print" (FTypeB Nothing "Int" []) condVS tce
    checkPrintableType vs1 tce
    checkPrintableType vs2 tce
checkPrintableType (FTValueStatement _ ts) tce = forM_ ts (`checkPrintableType` tce)
checkPrintableType (FFValueStatement (Just loc) _ _) _ = fail $ "lambda (at " ++ show loc ++ ") is not printable"
checkPrintableType (FFValueStatement Nothing _ _) _ = undefined
checkPrintableType (FExpr _ (FEAdd vs1 vs2)) tce = do
    checkPrintableType vs1 tce
    checkPrintableType vs2 tce
checkPrintableType (FExpr _ (FESub vs1 vs2)) tce = do
    checkPrintableType vs1 tce
    checkPrintableType vs2 tce
checkPrintableType (FExpr _ (FEMul vs1 vs2)) tce = do
    checkPrintableType vs1 tce
    checkPrintableType vs2 tce
checkPrintableType (FExpr _ (FEDiv vs1 vs2)) tce = do
    checkPrintableType vs1 tce
    checkPrintableType vs2 tce
checkPrintableType (FExpr _ (FEMod vs1 vs2)) tce = do
    checkPrintableType vs1 tce
    checkPrintableType vs2 tce
checkPrintableType (FExpr _ (FEL vs1 vs2)) tce = do
    checkPrintableType vs1 tce
    checkPrintableType vs2 tce
checkPrintableType (FExpr _ (FELQ vs1 vs2)) tce = do
    checkPrintableType vs1 tce
    checkPrintableType vs2 tce
checkPrintableType (FExpr _ (FEG vs1 vs2)) tce = do
    checkPrintableType vs1 tce
    checkPrintableType vs2 tce
checkPrintableType (FExpr _ (FEGQ vs1 vs2)) tce = do
    checkPrintableType vs1 tce
    checkPrintableType vs2 tce
checkPrintableType (FExpr _ (FEEQ vs1 vs2)) tce = do
    checkPrintableType vs1 tce
    checkPrintableType vs2 tce
checkPrintableType (FExpr _ (FENE vs1 vs2)) tce = do
    checkPrintableType vs1 tce
    checkPrintableType vs2 tce
checkPrintableType FRefAddr{} _ = undefined
checkPrintableType FSusValueStatement{} _ = undefined
checkPrintableType FSuspendedValue{} _ = undefined
checkPrintableType FSemaphore{} _ = undefined
checkPrintableType FNTValueStatement{} _ = undefined

checkFunctionApplicationType :: String -> FType -> String -> Maybe (Int, Int) -> [FValueStatement] -> TCE -> Err ()
checkFunctionApplicationType funName (FTypeT _ []) "print" _ [x] tce =
    checkPrintableType x tce -- (Int / Slist / List / Maybe / Either) of printable types
checkFunctionApplicationType funName (FTypeT _ []) "yield" _ [] _ = return ()
checkFunctionApplicationType funName (FTypeT _ []) "v" _ [x] tce =
    checkFunctionBody funName (FTypeB Nothing "Semaphore" []) x tce
checkFunctionApplicationType funName (FTypeT _ []) "p" _ [x] tce =
    checkFunctionBody funName (FTypeB Nothing "Semaphore" []) x tce
checkFunctionApplicationType funName t "get" _ [x] tce =
    checkFunctionBody funName (FTypeB Nothing "Ref" [t]) x tce
checkFunctionApplicationType funName (FTypeT _ []) "set" (Just pos) [FAValueStatement _ (FFunApplicationB _ name []), value] tce@(TCE tm atm) =
    if M.member name tm
        then
            case tm ! name of
                FTypeB _ "Ref" [t] ->
                    checkFunctionBody funName t value tce
                _ -> fail $ "the first argument of set at " ++ show pos ++ " is not a ref"
        else fail $ "use of undeclared function name " ++ name ++ " in function " ++ funName ++ " " ++ show pos
checkFunctionApplicationType funName (FTypeB _ "Ref" [FTypeB _ "Semaphore" []]) "make_semaphore" _ [] _ = return ()
checkFunctionApplicationType funName (FTypeB _ "SList" []) "getline" _ [] _ = return ()
checkFunctionApplicationType funName (FTypeB _ "SList" []) "gets" _ [x] tce =
    checkFunctionBody funName (FTypeB Nothing "Int" []) x tce
checkFunctionApplicationType funName t name (Just pos) args tce@(TCE tm atm) =
    if M.member name tm
        then checkFunctionApplicationTypeInt funName t name pos (tm ! name) args tce
        else fail $ "use of undeclared function name " ++ name ++ " in function " ++ funName ++ " " ++ show pos
checkFunctionApplicationType _ _ _ Nothing _ _ = undefined

checkTupleFunctionBody :: String -> [FType] -> [FValueStatement] -> TCE -> Err ()
checkTupleFunctionBody _ [] [FTValueStatement _ []] _ = return ()
checkTupleFunctionBody _ [] [] tce = return ()
checkTupleFunctionBody funName (t:types) (vs:vss) tce = do
    checkFunctionBody funName t vs tce
    checkTupleFunctionBody funName types vss tce
checkTupleFunctionBody _ a b _ = traceD (show a ++ show b) undefined

checkFunctionBody :: String -> FType -> FValueStatement -> TCE -> Err ()
checkFunctionBody funName (FunFType _ t1 t2) (FFValueStatement posM name vs) tce = do
    tce2 <- registerArg t1 (FPatternMatchB posM name) tce
    checkFunctionBody funName t2 vs tce2
checkFunctionBody funName t FFValueStatement{} _ = fail $ "function " ++ funName ++ " has a non function return type " ++ show t ++ " while it returns a function"
checkFunctionBody funName t (FForceValueStatement _ assignments vs) tce = do
    tce2 <- registerAssignments funName assignments tce
    checkFunctionBody funName t vs tce2
checkFunctionBody funName t (FAValueStatement _ ap@(FFunApplicationB pos name args)) tce =
    checkFunctionApplicationType funName t name pos args tce
checkFunctionBody funName t (FIfValueStatement posM ifvs vs1 vs2) tce = do
    checkFunctionBody funName (FTypeB posM "Int" []) ifvs tce
    checkFunctionBody funName t vs1 tce
    checkFunctionBody funName t vs2 tce
checkFunctionBody _ (FTypeB _ "Int" []) (FIValueStatement _ _) _ = return ()
checkFunctionBody funName (FTypeT _ [t]) vs tce = checkFunctionBody funName t vs tce
checkFunctionBody funName (FTypeT _ types) (FTValueStatement _ vss) tce = checkTupleFunctionBody funName types vss tce
checkFunctionBody funName t (FValueStatementB _ assignments vs) tce = do
    tce2 <- registerAssignments funName assignments tce
    checkFunctionBody funName t vs tce2
checkFunctionBody funName t@(FTypeB tPos atName atArgs) fc@(FCValueStatement pos cName cArgs) tce@(TCE tm atm) = do
    checkExistingType t tce
    let at@(FAlgType _ _ _ atvs) = atm ! atName
    atm <- checkConstructorExistence atName cName atvs
    (FAlgTypeVal _ _ atmArgs) <- getCorrectedConstructor atm at atArgs
    case atmArgs of
        tt@(FTypeT _ ts) ->
            if length ts == length cArgs 
                then checkTupleFunctionBody funName ts cArgs tce
                else case cArgs of
                    [x] -> checkFunctionBody funName tt x tce
                    x -> undefined
        _ -> checkTupleFunctionBody funName [atmArgs] cArgs tce
checkFunctionBody funName t@(FunFType _ t1 t2) vs@(FIValueStatement (Just posVS) _) _ =
    fail $ show vs ++ " " ++ show posVS ++ " is not of the type " ++ show t
checkFunctionBody funName t@FTypeB{} (FTValueStatement _ [vs]) tce = checkFunctionBody funName t vs tce
checkFunctionBody funName t@FTypeB{} vs@(FTValueStatement (Just pos) _) tce =
    fail $ "tuple at " ++ show pos ++ " in function " ++ funName ++ " has a non tuple type" ++ show t ++ show vs
checkFunctionBody _ _ (FAValueStatement _ FFunApplicationR{}) _ = undefined
checkFunctionBody funName t (FIValueStatement (Just loc) i) _ =
    fail $ "int value " ++ show i ++ " at " ++ show loc ++ " is not of the type Int ()"
checkFunctionBody _ _ (FIValueStatement Nothing _) _ = undefined
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
checkRefDefs [] _ = return ()
checkRefDefs (FRefDef _ t name vs:refDefs) tce = do
    checkFunctionBody name t vs tce
    checkRefDefs refDefs tce

checkTypes :: ProgramFormat -> Err (ProgramFormat, [String])
checkTypes pf@(SITList functionDefs refDefs algTypes) = do
    let tce0 = registerAlgTypes algTypes newTCE
    let tce1 = registerFunctionDefs functionDefs tce0
    let tce2 = registerRefDefs refDefs tce1
    checkFunctionDefs functionDefs tce2
    checkRefDefs refDefs tce2
    return (pf, [])