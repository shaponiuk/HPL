module StaticCheck.InitialConvertToProgramFormat where

import StaticCheck.Format
import Bnfc.AbsHpl
import Util.State
import Data.Char
import Debug.Trace

initialConvertToProgramFormat :: Program -> ProgramFormat
initialConvertToProgramFormat p = 
    SITList 
        (gatherAndConvertFunctionDefs p)
        (gatherAndConvertRefDefs p)
        (gatherAndConvertAlgTypes p)

gatherAndConvertFunctionDefs :: Program -> [FFunctionDef]
gatherAndConvertFunctionDefs p = map convertFunctionDef $ gatherFunctionDefs p

gatherFunctionDefs :: Program -> [FunctionDef]
gatherFunctionDefs (ProgramB l) = map sitToFunDef $ filter checkFunctionDef l

gatherAndConvertRefDefs :: Program -> [FRefDef]
gatherAndConvertRefDefs p = map convertRefDef $ gatherRefDefs p

gatherRefDefs :: Program -> [RefDef]
gatherRefDefs (ProgramB l) = map sitToRef $ filter checkRefDef l

gatherAndConvertAlgTypes :: Program -> [FAlgType]
gatherAndConvertAlgTypes p = map convertAlgType $ gatherAlgTypes p

gatherAlgTypes :: Program -> [AlgType]
gatherAlgTypes (ProgramB l) = map sitToAlgType $ filter checkAlgType l

sitToAlgType :: FunctionOrRefOrType -> AlgType
sitToAlgType (FunctionOrRefOrTypeT t) = t

sitToRef :: FunctionOrRefOrType -> RefDef
sitToRef (FunctionOrRefOrTypeR r) = r

sitToFunDef :: FunctionOrRefOrType -> FunctionDef
sitToFunDef (FunctionOrRefOrTypeF f) = f

checkFunctionDef :: FunctionOrRefOrType -> Bool
checkFunctionDef (FunctionOrRefOrTypeF _) = True
checkFunctionDef _ = False

checkRefDef :: FunctionOrRefOrType -> Bool
checkRefDef (FunctionOrRefOrTypeR _) = True
checkRefDef _ = False

checkAlgType :: FunctionOrRefOrType -> Bool
checkAlgType (FunctionOrRefOrTypeT _) = True
checkAlgType _ = False

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

unwrapIdent :: Ident -> String
unwrapIdent (Ident str) = str

convertIdentList :: [Ident] -> [String]
convertIdentList = map unwrapIdent

convertFunctionDef :: FunctionDef -> FFunctionDef
convertFunctionDef (FunctionDefB t ident argList vs) = 
  NonSusFFunctionDef (convertType t) (unwrapIdent ident) (convertArgList argList) (convertValueStatement vs)
convertFunctionDef (SusFunctionDef t ident argList vs) = 
  SusFFunctionDef $ NonSusFFunctionDef (convertType t) (unwrapIdent ident) (convertArgList argList) (convertValueStatement vs)

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
convertPatternMatchList [TPatternMatch []] = []
convertPatternMatchList l = map convertPatternMatch l

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
  if checkFunAppl funAppl
    then FAValueStatement $ convertFunctionAppl funAppl
    else 
      let
        (name, vss) = convertFunctionApplToTypeConstructor funAppl
      in FCValueStatement name vss
convertValueStatement (IValueStatement i) = FIValueStatement $ fromIntegral i
convertValueStatement (LitStrValueStatement str) = FLitStrValueStatement str
convertValueStatement (FValueStatement ident vs) = 
  FFValueStatement (unwrapIdent ident) (convertValueStatement vs)
convertValueStatement (Expr vs e) = exprFromLists (makeExprLists vs e)

checkFunAppl :: FunApplication -> Bool
checkFunAppl (FunApplicationB name _) = isLower $ head $ unwrapIdent name

convertFunctionApplToTypeConstructor :: FunApplication -> (String, [FValueStatement])
convertFunctionApplToTypeConstructor (FunApplicationB name functionArgAppls) = 
  (unwrapIdent name, map convertFunctionArgApplToTypeConstructor functionArgAppls)
convertFunctionApplToTypeConstructor a = trace (show a) undefined

convertFunctionArgApplToTypeConstructor :: FunctionArgAppl -> FValueStatement
convertFunctionArgApplToTypeConstructor (FunctionArgApplB vs) = convertValueStatement vs

exprFromLists :: ([String], [ValueStatement]) -> FValueStatement
exprFromLists (strs, vss) = let
    fvss = convertValueStatementList vss
  in mergeCmpVss $ mergeAddSubVss $ mergeMulDivModVss (strs, fvss)

mergeMulDivModVss :: ([String], [FValueStatement]) -> ([String], [FValueStatement])
mergeMulDivModVss x = let
    xaux = mergeMulDivModVssAux x
  in if x == xaux then x else mergeMulDivModVss xaux

mergeMulDivModVssAux :: ([String], [FValueStatement]) -> ([String], [FValueStatement])
mergeMulDivModVssAux ("*":strs, x:x2:xs) = (nstrs, (FExpr $ FEMul x x2):nxs) where
  (nstrs, nxs) = mergeMulDivModVssAux (strs, xs)
mergeMulDivModVssAux ("/":strs, x:x2:xs) = (nstrs, (FExpr $ FEDiv x x2):nxs) where
  (nstrs, nxs) = mergeMulDivModVssAux (strs, xs)
mergeMulDivModVssAux ("%":strs, x:x2:xs) = (nstrs, (FExpr $ FEMod x x2):nxs) where
  (nstrs, nxs) = mergeMulDivModVssAux (strs, xs)
mergeMulDivModVssAux (s:strs, x:xs) = (s:nstrs, x:nxs) where
  (nstrs, nxs) = mergeMulDivModVssAux (strs, xs)
mergeMulDivModVssAux x = x

mergeAddSubVss :: ([String], [FValueStatement]) -> ([String], [FValueStatement])
mergeAddSubVss x = let
    xaux = mergeAddSubVssAux x
  in if x == xaux then x else mergeAddSubVss xaux

mergeAddSubVssAux :: ([String], [FValueStatement]) -> ([String], [FValueStatement])
mergeAddSubVssAux ("+":strs, x:x2:xs) = (nstrs, (FExpr $ FEAdd x x2):nxs) where
  (nstrs, nxs) = mergeAddSubVssAux (strs, xs)
mergeAddSubVssAux ("-":strs, x:x2:xs) = (nstrs, (FExpr $ FESub x x2):nxs) where
  (nstrs, nxs) = mergeAddSubVssAux (strs, xs)
mergeAddSubVssAux (s:strs, x:xs) = (s:nstrs, x:nxs) where
  (nstrs, nxs) = mergeAddSubVssAux (strs, xs)
mergeAddSubVssAux x = x

mergeCmpVss :: ([String], [FValueStatement]) -> FValueStatement
mergeCmpVss ([], [x]) = x
mergeCmpVss ("<":strs, x:xs) = FExpr $ FEL x $ mergeCmpVss (strs, xs)
mergeCmpVss ("<=":strs, x:xs) = FExpr $ FELQ x $ mergeCmpVss (strs, xs)
mergeCmpVss (">":strs, x:xs) = FExpr $ FEG x $ mergeCmpVss (strs, xs)
mergeCmpVss (">=":strs, x:xs) = FExpr $ FEGQ x $ mergeCmpVss (strs, xs)
mergeCmpVss ("==":strs, x:xs) = FExpr $ FEEQ x $ mergeCmpVss (strs, xs)
mergeCmpVss ("!=":strs, x:xs) = FExpr $ FENE x $ mergeCmpVss (strs, xs)

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