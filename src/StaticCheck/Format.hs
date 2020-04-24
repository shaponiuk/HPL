module StaticCheck.Format where

data ProgramFormat = SITList [SIT]
  deriving (Show)

data SIT = SITStruct FStruct | SITInterface FInterface | SITType FAlgType
  deriving (Show)

data FStruct = FStructB String FStructBody | FStructI String [String] FStructBody
  deriving (Show)

data FInterface = 
    FInterfaceB String FInterfaceBody 
  | FInterfaceI String [String] FInterfaceBody
  deriving (Show)
 
data FAlgType = FAlgType String [String] [FAlgTypeVal]
  deriving (Show)

data FStructBody = FStructBody [FStructField]
  deriving (Show)

data FStructField = 
    FStructFieldFunPrivate FFunctionDef
  | FStructFieldFunPublic FFunctionDef
  | FStructFieldRefPrivate FRefDef
  | FStructFieldRefPublic FRefDef
  deriving (Show)

data FFunctionDef =
    NonSusFFunctionDef FType String [FFunctionArg] FValueStatement
  | SusFFunctionDef FFunctionDef
  deriving (Show)

data FRefDef = FRefDef FType String FValueStatement
  deriving (Show)

data FFunctionArg = FFunctionArg FPatternMatch
  deriving (Show)

data FPatternMatch =
    FPatternMatchI Int
  | FPatternMatchB String
  | FPatternMatchT [FPatternMatch]
  | FPatternMatchC FPatternMatch [FPatternMatch]
  deriving (Show)

data FType =
    FTypeB String [FType]
  | FunFType FType FType
  | FTypeT [FType]
  deriving (Show)

data FValueStatement =
    FValueStatementB [FAssignment] FValueStatement
  | FForceValueStatement [FAssignment] FValueStatement
  | FIfValueStatement FValueStatement FValueStatement FValueStatement
  | FLValueStatement [FValueStatement]
  | FTValueStatement [FValueStatement]
  | FAValueStatement FFunApplication
  | FIValueStatement Int
  | FLitStrValueStatement String
  | FFValueStatement String FValueStatement
  | FExpr FValueStatementExpr
  deriving (Show)

data FFunApplication =
    FSFunApplication String FFunApplication
  | FFunApplicationB String [FFunctionArgAppl]
  deriving (Show)

data FFunctionArgAppl = FFunctionArgAppl FValueStatement
  deriving (Show)

data FInterfaceBody = FInterfaceBody [FFunOrRefDecl]
  deriving (Show)

data FFunOrRefDecl =
    FFunOrRefDeclF FType String
  | FFunOrRefDeclSF FType String
  | FFunOrRefDeclR FType String
  deriving (Show)

data FAssignment =
    FAssignmentB FType FPatternMatch FValueStatement
  | FRefAssignment FRefDef
  deriving (Show)

data FAlgTypeVal = FAlgTypeVal String FType
  deriving (Show)

data FValueStatementExpr =
    FEAdd FValueStatement FValueStatement
  | FESub FValueStatement FValueStatement
  | FEMod FValueStatement FValueStatement
  | FEMul FValueStatement FValueStatement
  | FEDiv FValueStatement FValueStatement
  | FEL FValueStatement FValueStatement
  | FELQ FValueStatement FValueStatement
  | FEG FValueStatement FValueStatement
  | FEGQ FValueStatement FValueStatement
  | FEEQ FValueStatement FValueStatement
  | FENE FValueStatement FValueStatement
  deriving (Show)