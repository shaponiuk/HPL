module StaticCheck.InitialConvertToProgramFormat where

import StaticCheck.Format
import Bnfc.AbsHpl

initialConvertToProgramFormat :: Program -> ProgramFormat
initialConvertToProgramFormat p@(ProgramB l) = 
    SITList 
        (gatherAndConvertStructs p) 
        (gatherAndConvertInterfaces p) 
        (gatherAndConvertAlgTypes p)

gatherAndConvertStructs :: Program -> [FStruct]
gatherAndConvertStructs p@(ProgramB _) = map convertStruct $ gatherStructs p

gatherAndConvertInterfaces :: Program -> [FInterface]
gatherAndConvertInterfaces p@(ProgramB _) = map convertInterface $ gatherInterfaces p

gatherAndConvertAlgTypes :: Program -> [FAlgType]
gatherAndConvertAlgTypes p@(ProgramB _) = map convertAlgType $ gatherAlgTypes p

gatherStructs :: Program -> [Struct]
gatherStructs (ProgramB l) = map sitToStruct $ filter checkStruct l

gatherInterfaces :: Program -> [Interface]
gatherInterfaces (ProgramB l) = map sitToInterface $ filter checkInterface l

gatherAlgTypes :: Program -> [AlgType]
gatherAlgTypes (ProgramB l) = map sitToAlgType $ filter checkAlgType l

sitToStruct :: StructOrInterfaceOrType -> Struct
sitToStruct (StructOrInterfaceOrTypeS s) = s

sitToInterface :: StructOrInterfaceOrType -> Interface
sitToInterface (StructOrInterfaceOrTypeI i) = i

sitToAlgType :: StructOrInterfaceOrType -> AlgType
sitToAlgType (StructOrInterfaceOrTypeT t) = t

checkStruct :: StructOrInterfaceOrType -> Bool
checkStruct (StructOrInterfaceOrTypeS _) = True
checkStruct _ = False

checkInterface :: StructOrInterfaceOrType -> Bool
checkInterface (StructOrInterfaceOrTypeI _) = True
checkInterface _ = False

checkAlgType :: StructOrInterfaceOrType -> Bool
checkAlgType (StructOrInterfaceOrTypeT _) = True
checkAlgType _ = False

convertStruct :: Struct -> FStruct
convertStruct (StructB (Ident name) body) = 
  FStructB name $ convertBody body
convertStruct (StructI (Ident name) interfaceIdList body) = 
  FStructI name (convertInterfaceIdList interfaceIdList) $ convertBody body

convertInterface :: Interface -> FInterface
convertInterface (InterfaceB iid body) = 
  FInterfaceB (convertInterfaceId iid) (convertInterfaceBody body)
convertInterface (InterfaceBInh iid iidList body) = 
  FInterfaceI (convertInterfaceId iid) (convertInterfaceIdList iidList) (convertInterfaceBody body)

convertInterfaceBody :: InterfaceBody -> FInterfaceBody
convertInterfaceBody (InterfaceBodyB l) = FInterfaceBody $ map convertFunOrRefDecl l

convertFunOrRefDecl :: FunOrRefDecl -> FFunOrRefDecl
convertFunOrRefDecl (FunOrRefDeclF t i) = FFunOrRefDeclF (convertType t) (unwrapIdent i)
convertFunOrRefDecl (FunOrRefDeclSF t i) = FFunOrRefDeclSF (convertType t) (unwrapIdent i)
convertFunOrRefDecl (FunOrRefDeclR t i) = FFunOrRefDeclR (convertType t) (unwrapIdent i)

convertAlgType :: AlgType -> FAlgType
convertAlgType (AlgTypeB i taList atvList) = 
  FAlgType (unwrapIdent i) (convertTypeArgList taList) (convertAlgTypeValList atvList)

convertTypeArg :: TypeArg -> String
convertTypeArg (TypeArgB i) = unwrapIdent i

convertTypeArgList :: [TypeArg] -> [String]
convertTypeArgList = map convertTypeArg

convertAlgTypeVal :: AlgTypeVal -> FAlgTypeVal
convertAlgTypeVal (AlgTypeValB i t) = FAlgTypeVal (unwrapIdent i) (convertType t)

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
convertBody (StructBodyB l) = FStructBody $ map convertStructField l

convertStructField :: StructField -> FStructField
convertStructField (StructFieldFunPr fd) = FStructFieldFunPrivate $ convertFunctionDef fd
convertStructField (StructFieldFunPu fd) = FStructFieldFunPublic $ convertFunctionDef fd
convertStructField (StructFieldRefPr rd) = FStructFieldRefPrivate $ convertRefDef rd
convertStructField (StructFieldRefPu rd) = FStructFieldRefPublic $ convertRefDef rd

convertFunctionDef :: FunctionDef -> FFunctionDef
convertFunctionDef (FunctionDefB t ident argList vs) = 
  NonSusFFunctionDef (convertType t) (unwrapIdent ident) (convertArgList argList) (convertValueStatement vs)
convertFunctionDef (SusFunctionDef fd) = SusFFunctionDef $ convertFunctionDef fd

convertType :: Type -> FType
convertType (TypeB i tl) = FTypeB (unwrapIdent i) (convertTypeList tl)
convertType (FunType argType resType) = FunFType (convertType argType) (convertType resType)
convertType (TType tl) = FTypeT $ convertTypeList tl

convertTypeList :: [Type] -> [FType]
convertTypeList = map convertType

convertFunctionArg :: FunctionArg -> FPatternMatch
convertFunctionArg (FunctionArgB pm) = convertPatternMatch pm

convertPatternMatch :: PatternMatch -> FPatternMatch
convertPatternMatch (PatternMatchI i) = FPatternMatchI $ fromIntegral i
convertPatternMatch (PatternMatchB i) = FPatternMatchB $ unwrapIdent i
convertPatternMatch (TPatternMatch l) = 
  FPatternMatchT $ convertPatternMatchList l
convertPatternMatch (CPatternMatch pm l) = 
  FPatternMatchC (convertPatternMatch pm) (convertPatternMatchList l)

convertPatternMatchList :: [PatternMatch] -> [FPatternMatch]
convertPatternMatchList = map convertPatternMatch

convertArgList :: [FunctionArg] -> [FPatternMatch]
convertArgList = map convertFunctionArg

convertValueStatement :: ValueStatement -> FValueStatement
convertValueStatement (ValueStatementB assignments vs) = 
  FValueStatementB (convertAssignmentList assignments) (convertValueStatement vs)
convertValueStatement (ForceValueStatement assignements vs) =
  FForceValueStatement (convertAssignmentList assignements) (convertValueStatement vs)
convertValueStatement (IfValueStatement condvs ifvs elsevs) =
  FIfValueStatement (convertValueStatement condvs) (convertValueStatement ifvs) (convertValueStatement elsevs)
convertValueStatement (LValueStatement (ListValueStatementB l)) = 
  FLValueStatement $ convertValueStatementList l
convertValueStatement (TValueStatement (TupleValueStatementB l)) = 
  FTValueStatement $ convertValueStatementList l
convertValueStatement (AValueStatement funAppl) = 
  FAValueStatement $ convertFunctionAppl funAppl
convertValueStatement (IValueStatement i) = FIValueStatement $ fromIntegral i
convertValueStatement (LitStrValueStatement str) = FLitStrValueStatement str
convertValueStatement (FValueStatement ident vs) = 
  FFValueStatement (unwrapIdent ident) (convertValueStatement vs)
convertValueStatement (Expr vs e) = exprFromLists (makeExprLists vs e)

exprFromLists :: ([String], [ValueStatement]) -> FValueStatement
exprFromLists (strs, vss) = let
    fvss = convertValueStatementList vss
  in mergeCmpVss $ mergeAddSubVss $ mergeMulDivModVss (strs, fvss)

mergeMulDivModVss :: ([String], [FValueStatement]) -> ([String], [FValueStatement])
mergeMulDivModVss x = let
    xaux = mergeMulDivModVssAux x
  in if x == xaux then x else mergeMulDivModVss xaux

mergeMulDivModVssAux :: ([String], [FValueStatement]) -> ([String], [FValueStatement])
mergeMulDivModVssAux ("*":strs, (x:x2:xs)) = (nstrs, (FExpr $ FEMul x x2):nxs) where
  (nstrs, nxs) = mergeMulDivModVssAux (strs, xs)
mergeMulDivModVssAux ("/":strs, (x:x2:xs)) = (nstrs, (FExpr $ FEDiv x x2):nxs) where
  (nstrs, nxs) = mergeMulDivModVssAux (strs, xs)
mergeMulDivModVssAux ("%":strs, (x:x2:xs)) = (nstrs, (FExpr $ FEMod x x2):nxs) where
  (nstrs, nxs) = mergeMulDivModVssAux (strs, xs)
mergeMulDivModVssAux (s:strs, x:xs) = (s:nstrs, x:nxs) where
  (nstrs, nxs) = mergeMulDivModVssAux (strs, xs)
mergeMulDivModVssAux x = x

mergeAddSubVss :: ([String], [FValueStatement]) -> ([String], [FValueStatement])
mergeAddSubVss x = let
    xaux = mergeAddSubVssAux x
  in if x == xaux then x else mergeAddSubVss xaux

mergeAddSubVssAux :: ([String], [FValueStatement]) -> ([String], [FValueStatement])
mergeAddSubVssAux ("+":strs, (x:x2:xs)) = (nstrs, (FExpr $ FEAdd x x2):nxs) where
  (nstrs, nxs) = mergeAddSubVssAux (strs, xs)
mergeAddSubVssAux ("-":strs, (x:x2:xs)) = (nstrs, (FExpr $ FESub x x2):nxs) where
  (nstrs, nxs) = mergeAddSubVssAux (strs, xs)
mergeAddSubVssAux (s:strs, x:xs) = (s:nstrs, x:nxs) where
  (nstrs, nxs) = mergeAddSubVssAux (strs, xs)
mergeAddSubVssAux x = x

mergeCmpVss :: ([String], [FValueStatement]) -> FValueStatement
mergeCmpVss ([], [x]) = x
mergeCmpVss (("<"):strs, x:xs) = FExpr $ FEL x $ mergeCmpVss (strs, xs)
mergeCmpVss (("<="):strs, x:xs) = FExpr $ FELQ x $ mergeCmpVss (strs, xs)
mergeCmpVss ((">"):strs, x:xs) = FExpr $ FEG x $ mergeCmpVss (strs, xs)
mergeCmpVss ((">="):strs, x:xs) = FExpr $ FEGQ x $ mergeCmpVss (strs, xs)
mergeCmpVss (("=="):strs, x:xs) = FExpr $ FEEQ x $ mergeCmpVss (strs, xs)
mergeCmpVss (("!="):strs, x:xs) = FExpr $ FENE x $ mergeCmpVss (strs, xs)

makeExprLists :: ValueStatement -> ValueStatementExpr -> ([String], [ValueStatement])
makeExprLists vs1 vs2 = (signs, vs1:vss) where
  (signs, vss) = makeExprListsAux vs2

insertPairToList :: (a, b) -> ([a], [b]) -> ([a], [b])
insertPairToList (a, b) (as, bs) = (a:as, b:bs)

makeExprListsAux :: ValueStatementExpr -> ([String], [ValueStatement])
makeExprListsAux (EAdd (Expr vs1 e)) = insertPairToList ("+", vs1) (makeExprListsAux e)
makeExprListsAux (ESub (Expr vs1 e)) = insertPairToList ("-", vs1) (makeExprListsAux e)
makeExprListsAux (EMod (Expr vs1 e)) = insertPairToList ("%", vs1) (makeExprListsAux e)
makeExprListsAux (EMul (Expr vs1 e)) = insertPairToList ("*", vs1) (makeExprListsAux e)
makeExprListsAux (EDiv (Expr vs1 e)) = insertPairToList ("/", vs1) (makeExprListsAux e)
makeExprListsAux (EL (Expr vs1 e)) = insertPairToList ("<", vs1) (makeExprListsAux e)
makeExprListsAux (ELQ (Expr vs1 e)) = insertPairToList ("<=", vs1) (makeExprListsAux e)
makeExprListsAux (EG (Expr vs1 e)) = insertPairToList (">", vs1) (makeExprListsAux e)
makeExprListsAux (EGQ (Expr vs1 e)) = insertPairToList (">=", vs1) (makeExprListsAux e)
makeExprListsAux (EEQ (Expr vs1 e)) = insertPairToList ("==", vs1) (makeExprListsAux e)
makeExprListsAux (ENE (Expr vs1 e)) = insertPairToList ("!=", vs1) (makeExprListsAux e)
makeExprListsAux (EAdd vs) = (["+"], [vs])
makeExprListsAux (ESub vs) = (["-"], [vs])
makeExprListsAux (EMod vs) = (["%"], [vs])
makeExprListsAux (EMul vs) = (["*"], [vs])
makeExprListsAux (EDiv vs) = (["/"], [vs])
makeExprListsAux (EL vs) = (["<"], [vs])
makeExprListsAux (ELQ vs) = (["<="], [vs])
makeExprListsAux (EG vs) = ([">"], [vs])
makeExprListsAux (EGQ vs) = ([">="], [vs])
makeExprListsAux (EEQ vs) = (["=="], [vs])
makeExprListsAux (ENE vs) = (["!="], [vs])

convertFunctionAppl :: FunApplication -> FFunApplication
convertFunctionAppl (FunApplicationB i fArgApplList) = FFunApplicationB (unwrapIdent i) (convertFunctionArgApplList fArgApplList)
convertFucntionAppl (SFunApplication i fAppl) = FSFunApplication (unwrapIdent i) (convertFunctionAppl fAppl)

convertFunctionArgAppl :: FunctionArgAppl -> FValueStatement
convertFunctionArgAppl (FunctionArgApplB vs) = convertValueStatement vs

convertFunctionArgApplList :: [FunctionArgAppl] -> [FValueStatement]
convertFunctionArgApplList = map convertFunctionArgAppl
 
convertFunctionArgList :: [FunctionArg] -> [FPatternMatch]
convertFunctionArgList = map convertFunctionArg

convertValueStatementList :: [ValueStatement] -> [FValueStatement]
convertValueStatementList = map convertValueStatement

convertAssignment :: Assignment -> FAssignment
convertAssignment (AssignmentB t pm vs) = FAssignmentB (convertType t) (convertPatternMatch pm) (convertValueStatement vs)
convertAssignment (RefAssignment refdef) = FRefAssignment $ convertRefDef refdef

convertAssignmentList :: [Assignment] -> [FAssignment]
convertAssignmentList = map convertAssignment

convertRefDef :: RefDef -> FRefDef
convertRefDef (RefDefB t i vs) = FRefDef (convertType t) (unwrapIdent i) (convertValueStatement vs)