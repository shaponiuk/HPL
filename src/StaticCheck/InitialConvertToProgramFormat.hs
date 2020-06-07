module StaticCheck.InitialConvertToProgramFormat where

import StaticCheck.Format
import Bnfc.AbsHpl
import Util.State
import Data.Char
import Debug.Trace

initialConvertToProgramFormat :: Program (Maybe (Int, Int)) -> ProgramFormat
initialConvertToProgramFormat p = 
    SITList 
        (gatherAndConvertFunctionDefs p)
        (gatherAndConvertRefDefs p)
        (gatherAndConvertAlgTypes p)

gatherAndConvertFunctionDefs :: Program (Maybe (Int, Int)) -> [FFunctionDef]
gatherAndConvertFunctionDefs p = map convertFunctionDef $ gatherFunctionDefs p

gatherFunctionDefs :: Program (Maybe (Int, Int)) -> [FunctionDef (Maybe (Int, Int))]
gatherFunctionDefs (ProgramB _ l) = map sitToFunDef $ filter checkFunctionDef l

gatherAndConvertRefDefs :: Program (Maybe (Int, Int)) -> [FRefDef]
gatherAndConvertRefDefs p = map convertRefDef $ gatherRefDefs p

gatherRefDefs :: Program (Maybe (Int, Int)) -> [RefDef (Maybe (Int, Int))]
gatherRefDefs (ProgramB _ l) = map sitToRef $ filter checkRefDef l

gatherAndConvertAlgTypes :: Program (Maybe (Int, Int)) -> [FAlgType]
gatherAndConvertAlgTypes p = map convertAlgType $ gatherAlgTypes p

gatherAlgTypes :: Program (Maybe (Int, Int)) -> [AlgType (Maybe (Int, Int))]
gatherAlgTypes (ProgramB _ l) = map sitToAlgType $ filter checkAlgType l

sitToAlgType :: FunctionOrRefOrType (Maybe (Int, Int)) -> AlgType (Maybe (Int, Int))
sitToAlgType (FunctionOrRefOrTypeT _ t) = t
sitToAlgType _ = undefined

sitToRef :: FunctionOrRefOrType (Maybe (Int, Int)) -> RefDef (Maybe (Int, Int))
sitToRef (FunctionOrRefOrTypeR _ r) = r
sitToRef _ = undefined

sitToFunDef :: FunctionOrRefOrType (Maybe (Int, Int)) -> FunctionDef (Maybe (Int, Int))
sitToFunDef (FunctionOrRefOrTypeF _ f) = f
sitToFunDef _ = undefined

checkFunctionDef :: FunctionOrRefOrType (Maybe (Int, Int)) -> Bool
checkFunctionDef (FunctionOrRefOrTypeF _ _) = True
checkFunctionDef _ = False

checkRefDef :: FunctionOrRefOrType (Maybe (Int, Int)) -> Bool
checkRefDef (FunctionOrRefOrTypeR _ _) = True
checkRefDef _ = False

checkAlgType :: FunctionOrRefOrType (Maybe (Int, Int)) -> Bool
checkAlgType (FunctionOrRefOrTypeT _ _) = True
checkAlgType _ = False

convertAlgType :: AlgType (Maybe (Int, Int)) -> FAlgType
convertAlgType (AlgTypeB pos i taList atvList) = 
  FAlgType pos (unwrapIdent i) (convertTypeArgList taList) (convertAlgTypeValList atvList)

convertTypeArg :: TypeArg (Maybe (Int, Int)) -> String
convertTypeArg (TypeArgB _ i) = unwrapIdent i

convertTypeArgList :: [TypeArg (Maybe (Int, Int))] -> [String]
convertTypeArgList = map convertTypeArg

convertAlgTypeVal :: AlgTypeVal (Maybe (Int, Int)) -> FAlgTypeVal
convertAlgTypeVal (AlgTypeValB pos i t) = FAlgTypeVal pos (unwrapIdent i) (convertType t)

convertAlgTypeValList :: [AlgTypeVal (Maybe (Int, Int))] -> [FAlgTypeVal]
convertAlgTypeValList = map convertAlgTypeVal

unwrapIdent :: Ident -> String
unwrapIdent (Ident str) = str

convertIdentList :: [Ident] -> [String]
convertIdentList = map unwrapIdent

convertFunctionDef :: FunctionDef (Maybe (Int, Int)) -> FFunctionDef
convertFunctionDef (FunctionDefB pos t ident argList vs) = 
  NonSusFFunctionDef pos (convertType t) (unwrapIdent ident) (convertArgList argList) (convertValueStatement vs)
convertFunctionDef (SusFunctionDef pos t ident argList vs) = 
  SusFFunctionDef $ NonSusFFunctionDef pos (convertType t) (unwrapIdent ident) (convertArgList argList) (convertValueStatement vs)

convertType :: Type (Maybe (Int, Int)) -> FType
convertType (TypeB pos i tl) = FTypeB pos (unwrapIdent i) (convertTypeList tl)
convertType (FunType pos argType resType) = FunFType pos (convertType argType) (convertType resType)
convertType (TType _ [t]) = convertType t
convertType (TType pos tl) = FTypeT pos $ convertTypeList tl

convertTypeList :: [Type (Maybe (Int, Int))] -> [FType]
convertTypeList = map convertType

convertFunctionArg :: FunctionArg (Maybe (Int, Int)) -> FPatternMatch
convertFunctionArg (FunctionArgB _ pm) = convertPatternMatch pm

convertPatternMatch :: PatternMatch (Maybe (Int, Int)) -> FPatternMatch
convertPatternMatch (PatternMatchI pos i) = FPatternMatchI pos $ fromIntegral i
convertPatternMatch (PatternMatchB pos i) = FPatternMatchB pos $ unwrapIdent i
convertPatternMatch (TPatternMatch _ [pm]) = convertPatternMatch pm
convertPatternMatch (TPatternMatch pos l) = 
  FPatternMatchT pos $ convertPatternMatchList l
convertPatternMatch (CPatternMatch pos pm l) = 
  FPatternMatchC pos (convertPatternMatch pm) (convertPatternMatchList l)

convertPatternMatchList :: [PatternMatch (Maybe (Int, Int))] -> [FPatternMatch]
convertPatternMatchList [TPatternMatch _ []] = []
convertPatternMatchList l = map convertPatternMatch l

convertArgList :: [FunctionArg (Maybe (Int, Int))] -> [FPatternMatch]
convertArgList = map convertFunctionArg

convertValueStatement :: ValueStatement (Maybe (Int, Int)) -> FValueStatement
convertValueStatement (ValueStatementB pos assignments vs) = 
  FValueStatementB pos (convertAssignmentList assignments) (convertValueStatement vs)
convertValueStatement (ForceValueStatement pos assignements vs) =
  FForceValueStatement pos (convertAssignmentList assignements) (convertValueStatement vs)
convertValueStatement (IfValueStatement pos condvs ifvs elsevs) =
  FIfValueStatement pos (convertValueStatement condvs) (convertValueStatement ifvs) (convertValueStatement elsevs)
convertValueStatement (TValueStatement _ (TupleValueStatementB _ [t])) =
  convertValueStatement t
convertValueStatement (TValueStatement pos (TupleValueStatementB _ l)) = 
  FTValueStatement pos $ convertValueStatementList l
convertValueStatement (AValueStatement pos funAppl) = 
  if checkFunAppl funAppl
    then FAValueStatement pos $ convertFunctionAppl funAppl
    else 
      let
        (pos, name, vss) = convertFunctionApplToTypeConstructor funAppl
      in FCValueStatement pos name vss
convertValueStatement (IValueStatement pos i) = FIValueStatement pos $ fromIntegral i
convertValueStatement (LitStrValueStatement pos str) = convertString pos str
convertValueStatement (FValueStatement pos ident vs) = 
  FFValueStatement pos (unwrapIdent ident) (convertValueStatement vs)
convertValueStatement (Expr pos vs e) = exprFromLists pos (makeExprLists vs e)

checkFunAppl :: FunApplication (Maybe (Int, Int)) -> Bool
checkFunAppl (FunApplicationB _ name _) = isLower $ head $ unwrapIdent name

convertFunctionApplToTypeConstructor :: FunApplication (Maybe (Int, Int)) -> (Maybe (Int, Int), String, [FValueStatement])
convertFunctionApplToTypeConstructor (FunApplicationB pos name functionArgAppls) = 
  (pos, unwrapIdent name, map convertFunctionArgApplToTypeConstructor functionArgAppls)

convertFunctionArgApplToTypeConstructor :: FunctionArgAppl (Maybe (Int, Int)) -> FValueStatement
convertFunctionArgApplToTypeConstructor (FunctionArgApplB _ vs) = convertValueStatement vs

exprFromLists :: Maybe (Int, Int) -> ([String], [ValueStatement (Maybe (Int, Int))]) -> FValueStatement
exprFromLists pos (strs, vss) = let
    fvss = convertValueStatementList vss
  in mergeCmpVss $ mergeAddSubVss $ mergeMulDivModVss (strs, fvss)

mergeMulDivModVss :: ([String], [FValueStatement]) -> ([String], [FValueStatement])
mergeMulDivModVss x = let
    xaux = mergeMulDivModVssAux x
  in if x == xaux then x else mergeMulDivModVss xaux

mergeMulDivModVssAux :: ([String], [FValueStatement]) -> ([String], [FValueStatement])
mergeMulDivModVssAux ("*":strs, x:x2:xs) = (nstrs, FExpr pos (FEMul x x2):nxs) where
  (nstrs, nxs) = mergeMulDivModVssAux (strs, xs)
  pos = getVSLoc x
mergeMulDivModVssAux ("/":strs, x:x2:xs) = (nstrs, FExpr pos (FEDiv x x2):nxs) where
  (nstrs, nxs) = mergeMulDivModVssAux (strs, xs)
  pos = getVSLoc x
mergeMulDivModVssAux ("%":strs, x:x2:xs) = (nstrs, FExpr pos (FEMod x x2):nxs) where
  (nstrs, nxs) = mergeMulDivModVssAux (strs, xs)
  pos = getVSLoc x
mergeMulDivModVssAux (s:strs, x:xs) = (s:nstrs, x:nxs) where
  (nstrs, nxs) = mergeMulDivModVssAux (strs, xs)
mergeMulDivModVssAux x = x

mergeAddSubVss :: ([String], [FValueStatement]) -> ([String], [FValueStatement])
mergeAddSubVss x = let
    xaux = mergeAddSubVssAux x
  in if x == xaux then x else mergeAddSubVss xaux

mergeAddSubVssAux :: ([String], [FValueStatement]) -> ([String], [FValueStatement])
mergeAddSubVssAux ("+":strs, x:x2:xs) = (nstrs, FExpr pos (FEAdd x x2):nxs) where
  (nstrs, nxs) = mergeAddSubVssAux (strs, xs)
  pos = getVSLoc x
mergeAddSubVssAux ("-":strs, x:x2:xs) = (nstrs, FExpr pos (FESub x x2):nxs) where
  (nstrs, nxs) = mergeAddSubVssAux (strs, xs)
  pos = getVSLoc x
mergeAddSubVssAux (s:strs, x:xs) = (s:nstrs, x:nxs) where
  (nstrs, nxs) = mergeAddSubVssAux (strs, xs)
mergeAddSubVssAux x = x

mergeCmpVss :: ([String], [FValueStatement]) -> FValueStatement
mergeCmpVss ([], [x]) = x
mergeCmpVss ("<":strs, x:xs) = FExpr pos $ FEL x $ mergeCmpVss (strs, xs) where pos = getVSLoc x
mergeCmpVss ("<=":strs, x:xs) = FExpr pos $ FELQ x $ mergeCmpVss (strs, xs) where pos = getVSLoc x
mergeCmpVss (">":strs, x:xs) = FExpr pos $ FEG x $ mergeCmpVss (strs, xs) where pos = getVSLoc x
mergeCmpVss (">=":strs, x:xs) = FExpr pos $ FEGQ x $ mergeCmpVss (strs, xs) where pos = getVSLoc x
mergeCmpVss ("==":strs, x:xs) = FExpr pos $ FEEQ x $ mergeCmpVss (strs, xs) where pos = getVSLoc x
mergeCmpVss ("!=":strs, x:xs) = FExpr pos $ FENE x $ mergeCmpVss (strs, xs) where pos = getVSLoc x
mergeCmpVss ([], []) = undefined
mergeCmpVss ([], _:_:_) = undefined
mergeCmpVss (_:_, _) = undefined

makeExprLists :: ValueStatement (Maybe (Int, Int)) -> ValueStatementExpr (Maybe (Int, Int)) -> ([String], [ValueStatement (Maybe (Int, Int))])
makeExprLists vs1 vs2 = (signs, vs1:vss) where
  (signs, vss) = makeExprListsAux vs2

insertPairToList :: (a, b) -> ([a], [b]) -> ([a], [b])
insertPairToList (a, b) (as, bs) = (a:as, b:bs)

makeExprListsAux :: ValueStatementExpr (Maybe (Int, Int)) -> ([String], [ValueStatement (Maybe (Int, Int))])
makeExprListsAux (EAdd _ (Expr _ vs1 e)) = insertPairToList ("+", vs1) (makeExprListsAux e)
makeExprListsAux (ESub _ (Expr _ vs1 e)) = insertPairToList ("-", vs1) (makeExprListsAux e)
makeExprListsAux (EMod _ (Expr _ vs1 e)) = insertPairToList ("%", vs1) (makeExprListsAux e)
makeExprListsAux (EMul _ (Expr _ vs1 e)) = insertPairToList ("*", vs1) (makeExprListsAux e)
makeExprListsAux (EDiv _ (Expr _ vs1 e)) = insertPairToList ("/", vs1) (makeExprListsAux e)
makeExprListsAux (EL _ (Expr _ vs1 e)) = insertPairToList ("<", vs1) (makeExprListsAux e)
makeExprListsAux (ELQ _ (Expr _ vs1 e)) = insertPairToList ("<=", vs1) (makeExprListsAux e)
makeExprListsAux (EG _ (Expr _ vs1 e)) = insertPairToList (">", vs1) (makeExprListsAux e)
makeExprListsAux (EGQ _ (Expr _ vs1 e)) = insertPairToList (">=", vs1) (makeExprListsAux e)
makeExprListsAux (EEQ _ (Expr _ vs1 e)) = insertPairToList ("==", vs1) (makeExprListsAux e)
makeExprListsAux (ENE _ (Expr _ vs1 e)) = insertPairToList ("!=", vs1) (makeExprListsAux e)
makeExprListsAux (EAdd _ vs) = (["+"], [vs])
makeExprListsAux (ESub _ vs) = (["-"], [vs])
makeExprListsAux (EMod _ vs) = (["%"], [vs])
makeExprListsAux (EMul _ vs) = (["*"], [vs])
makeExprListsAux (EDiv _ vs) = (["/"], [vs])
makeExprListsAux (EL _ vs) = (["<"], [vs])
makeExprListsAux (ELQ _ vs) = (["<="], [vs])
makeExprListsAux (EG _ vs) = ([">"], [vs])
makeExprListsAux (EGQ _ vs) = ([">="], [vs])
makeExprListsAux (EEQ _ vs) = (["=="], [vs])
makeExprListsAux (ENE _ vs) = (["!="], [vs])

convertFunctionAppl :: FunApplication (Maybe (Int, Int)) -> FFunApplication
convertFunctionAppl (FunApplicationB pos i fArgApplList) = FFunApplicationB pos (unwrapIdent i) (convertFunctionArgApplList fArgApplList)

convertFunctionArgAppl :: FunctionArgAppl (Maybe (Int, Int)) -> FValueStatement
convertFunctionArgAppl (FunctionArgApplB _ vs) = convertValueStatement vs

convertFunctionArgApplList :: [FunctionArgAppl (Maybe (Int, Int))] -> [FValueStatement]
convertFunctionArgApplList = map convertFunctionArgAppl
 
convertFunctionArgList :: [FunctionArg (Maybe (Int, Int))] -> [FPatternMatch]
convertFunctionArgList = map convertFunctionArg

convertValueStatementList :: [ValueStatement (Maybe (Int, Int))] -> [FValueStatement]
convertValueStatementList = map convertValueStatement

convertAssignment :: Assignment (Maybe (Int, Int)) -> FAssignment
convertAssignment (AssignmentB pos t pm vs) = FAssignmentB pos (convertType t) (convertPatternMatch pm) (convertValueStatement vs)
convertAssignment (RefAssignment pos refdef) = FRefAssignment pos $ convertRefDef refdef

convertAssignmentList :: [Assignment (Maybe (Int, Int))] -> [FAssignment]
convertAssignmentList = map convertAssignment

convertRefDef :: RefDef (Maybe (Int, Int)) -> FRefDef
convertRefDef (RefDefB pos t i vs) = FRefDef pos (convertType t) (unwrapIdent i) (convertValueStatement vs)