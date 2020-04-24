module StaticCheck.StaticCheck where

import StaticCheck.Format
import Bnfc.AbsHpl
import Bnfc.ErrM

staticCheck :: Program -> Err ProgramFormat
staticCheck p = staticCheckErrPack $ staticCheckEither p

staticCheckErrPack :: Either String ProgramFormat -> Err ProgramFormat
staticCheckErrPack (Left e) = Bad e
staticCheckErrPack (Right r) = Ok r

staticCheckEither :: Program -> Either String ProgramFormat
staticCheckEither p = do
  let pf = initialConvertToProgramFormat p
  fail $ show pf
  return pf

initialConvertToProgramFormat :: Program -> ProgramFormat
initialConvertToProgramFormat ProgramB = SITList $ map absSITToSit

absSITToSit :: StructOrInterfaceOrType -> SIT
absSITToSit StructOrInterfaceOrTypeS = SITStruct convertStruct
absSITToSit StructOrInterfaceOrTypeI = SITInterface convertInterface
absSITToSit StructOrInterfaceOrTypeT = SITType convertAlgType

convertStruct :: Struct -> FStruct
convertStruct (StructB (Ident name) body) = 
  FStructB name $ convertBody body
convertStruct (StructI (Ident name) interfaceIdList body) = 
  FStructI name (convertInterfaceIdList interfaceIdList) $ convertBody body

convertInterface :: Interface -> FInterface
convertInterface (InterfaceB iid) = 
  FInterfaceB (convertInterfaceId iid) convertInterfaceBody
convertInterface (InterfaceBIng iid iidList) = 
  FInterfaceI (convertInterfaceId iid) (convertInterfaceIdList iid) convertInterfaceBody

convertInterfaceBody :: InterfaceBody -> FInterfaceBody
convertInterfaceBody InterfaceBodyB = FInterfaceBody $ map convertFunOrRefDecl

convertFunOrRefDecl :: FunOrRefDecl -> FFunOrRefDecl
convertFunOrRefDecl (FunOrRefDeclF t) = FFunOrRefDeclF (convertType t) unwrapIdent
convertFunOrRefDecl (FunOrRefDeclSF t) = FFunOrRefDeclSF (convertType t) unwrapIdent
convertFunOrRefDecl (FunOrRefDeclR t) = FFunOrRefDeclR (convertType t) unwrapIdent

convertAlgType :: AlgType -> FAlgType
convertAlgType (AlgTypeB i taList) = 
  FAlgType (unwrapIdent i) (convertTypeArgList taList) convertAlgTypeValList

convertTypeArg :: TypeArg -> String
convertTypeArg TypeArgB = unwrapIdent

convertTypeArgList :: [TypeArg] -> [String]
convertTypeArgList = map convertTypeArg

convertAlgTypeVal :: AlgTypeVal -> FAlgTypeVal
convertAlgTypeVal (AlgTypeValB i) = FAlgTypeVal (unwrapIdent i) convertType

convertAlgTypeValList :: [AlgTypeVal] -> [FAlgTypeVal]
convertAlgTypeValList = map convertAlgTypeVal

convertInterfaceIdList :: [InterfaceId] -> [String]
convertInterfaceIdList = map convertInterfaceId

convertInterfaceId :: InterfaceId -> String
convertInterfaceId = unwrapIdent . unwrapInterfaceId

unwrapInterfaceId :: InterfaceId -> Ident
unwrapInterfaceId (InterfaceIdB ident) = ident

unwrapIdent :: Ident -> String
unwrapIdent (Ident str) = str

convertIdentList :: [Ident] -> [String]
convertIdentList = map unwrapIdent

convertBody :: StructBody -> FStructBody
convertBody StructBodyB = FStructBody $ map convertStructField

convertStructField :: StructField -> FStructField
convertStructField StructFieldFunPr = FStructFieldFunPrivate convertFunctionDef
convertStructField StructFieldFunPu = FStructFieldFunPublic convertFunctionDef
convertStructField StructFieldRefPr = FStructFieldRefPrivate convertRefDef
convertStructField StructFieldRefPu = FStructFieldRefPublic convertRefDef

convertFunctionDef :: FunctionDef -> FFunctionDef
convertFunctionDef (FunctionDefB t ident argList vs) = 
  NonSusFFunctionDef (convertType t) (unwrapIdent ident) (convertArgList argList) (convertValueStatement vs)
convertFunctionDef SusFunctionDef = SusFFunctionDef convertFunctionDef

convertType :: Type -> FType
convertType (TypeB i) = FTypeB (unwrapIdent i) convertTypeList
convertType (FunType argType resType) = FunFType (convertType argType) (convertType resType)
convertType TType = FTypeT convertTypeList

convertTypeList :: [Type] -> [FType]
convertTypeList = map convertType

convertFunctionArg :: FunctionArg -> FFunctionArg
convertFunctionArg FunctionArgB = convertPatternMatch

convertPatternMatch :: PatternMatch -> FPatternMatch
convertPatternMatch PatternMatchI = FPatternMatchI
convertPatternMatch PatternMatchB = FPatternMatchB
convertPatternMatch TPatternMatch = FPatternMatchT convertPatternMatchList
convertPatternMatch (CPatternMatch pm) = FPatternMatchC (convertPatternMatch pm) convertPatternMatchList

convertPatternMatchList :: [PatternMatch] -> [FPatternMatch]
convertPatternMatchList = map convertPatternMatch

convertArgList :: [FunctionArg] -> [FFunctionArg]
convertArgList = map convertFunctionArg

convertValueStatement :: ValueStatement -> FValueStatement
convertValueStatement (ValueStatementB assignments) = 
  FValueStatementB (convertAssignmentList assignments) convertValueStatement
convertValueStatement (ForceValueStatement assignements) =
  FForceValueStatement (convertAssignmentList assignements) convertValueStatement
convertValueStatement (IfValueStatement condvs ifvs elsevs) =
  FIfValueStatement (convertValueStatement condvs) (convertValueStatement ifvs) (convertValueStatement elsevs)
convertValueStatement (LValueStatement ListValueStatementB) = 
  FLValueStatement convertValueStatementList
convertValueStatement (TValueStatement TupleValueStatementB) = 
  FTValueStatement convertValueStatementList
convertValueStatement AValueStatement = FAValueStatement convertFunctionAppl
convertValueStatement (IValueStatement i) = FIValueStatement i
convertValueStatement (LitStrValueStatement str) = FLitStrValueStatement str
convertValueStatement (FValueStatement ident) = FFValueStatement (unwrapIdent ident) convertValueStatement
convertValueStatement Expr = exprFromLists . makeExprLists

exprFromLists :: ([String], [ValueStatement]) -> FValueStatement
exprFromLists (strs, vss) = let
    fvss = convertValueStatementList vss
  in mergeCmpVss . mergeAddSubVss . mergeMulDivModVss fvss

mergeMulDivModVss :: ([String], [FValueStatement]) -> ([String], [FValueStatement])
mergeMulDivModVss x = let
    xaux = mergeMulDivModVssAux x
  in if x == xaux then x else mergeMulDivModVssAux xaux

mergeMulDivModVssAux :: ([String], [FValueStatement]) -> ([String], [FValueStatement])

mergeAddSubVss:: ([String], [FValueStatement]) -> ([String], [FValueStatement])

mergeCmpVss :: ([String], [FValueStatement]) -> FValueStatement
mergeCmpVss ([], [x]) = x
mergeCmpVss (("<"):strs, x:xs) = FExpr $ FEL x $ mergeCmpVss (strs, xs)
mergeCmpVss (("<="):strs, x:xs) = FExpr $ FELQ x $ mergeCmpVss (strs, xs)
mergeCmpVss ((">"):strs, x:xs) = FExpr $ FEG $ mergeCmpVss (strs, xs)
mergeCmpVss ((">="):strs, x:xs) = FExpr $ FEGQ x $ mergeCmpVss (strs, xs)
mergeCmpVss (("=="):strs, x:xs) = FExpr $ FEEQ x $ mergeCmpVss (strs, xs)
mergeCmpVss (("!="):strs, x:xs) = FExpr $ FENE x $ mergeCmpVss (strs, xs)

makeExprLists :: ValueStatement -> ValueStatement -> ([String], [ValueStatement])
makeExprLists vs1 vs2 = (signs, vs1:vss) where
  (signs, vss) = makeExprListsAux vs2

insertPairToList :: (a, b) -> ([a], [b]) -> ([a], [b])
insertPairToList (a, b) (as, bs) = (a:as, b:bs)

makeExprListsAux :: ValueStatement -> ([String], [ValueStatement])
makeExprListsAux vs@(Expr (EAdd vs2)) = insertPairToList ("+", vs) (makeExprListsAux vs2)
makeExprListsAux vs@(Expr (ESub vs2)) = insertPairToList ("-", vs) (makeExprListsAux vs2)
makeExprListsAux vs@(Expr (EMod vs2)) = insertPairToList ("%", vs) (makeExprListsAux vs2)
makeExprListsAux vs@(Expr (EMul vs2)) = insertPairToList ("*", vs) (makeExprListsAux vs2)
makeExprListsAux vs@(Expr (EDiv vs2)) = insertPairToList ("/", vs) (makeExprListsAux vs2)
makeExprListsAux vs@(Expr (EL vs2)) = insertPairToList ("<", vs) (makeExprListsAux vs2)
makeExprListsAux vs@(Expr (ELQ vs2)) = insertPairToList ("<=", vs) (makeExprListsAux vs2)
makeExprListsAux vs@(Expr (EG vs2)) = insertPairToList (">", vs) (makeExprListsAux vs2)
makeExprListsAux vs@(Expr (EGQ vs2)) = insertPairToList (">=", vs) (makeExprListsAux vs2)
makeExprListsAux vs@(Expr (EEQ vs2)) = insertPairToList ("==", vs) (makeExprListsAux vs2)
makeExprListsAux vs@(Expr (ENE vs2)) = insertPairToList ("!=", vs) (makeExprListsAux vs2)
makeExprListsAux _ = ([], [])

convertFunctionAppl :: FunApplication -> FFunApplication

convertValueStatementList :: [ValueStatement] -> [FValueStatement]
convertValueStatementList = map convertValueStatement

convertAssignment :: Assignment -> FAssignment

convertAssignmentList :: [Assignment] -> FAssignment
convertAssignmentList = map convertAssignement

convertRefDef :: RefDef -> FRefDef
convertRefDef = undefined