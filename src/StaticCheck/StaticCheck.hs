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

convertRefDef :: RefDef -> FRefDef
convertRefDef = undefined