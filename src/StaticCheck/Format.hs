module StaticCheck.Format where

data ProgramFormat = SITList [SIT]
  deriving (Show)

data SIT = SITStruct FStruct | SITInterface FInterface | SITType FAlgType
  deriving (Show)

data FStruct = FStructB String FStructBody | String  [String] FStructBody
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
    TypeB String [FType]
  | FunFType FType FType
  | FTypeT [FType]
  deriving (Show)

data FValueStatement =
    FValueStatementB [FAssignment] FValueStatement
  | FForceValueStatement [FAssignment] FValueStatement
  | FIfValueStatement FValueStatement FValueStatement FValueStatement
  | FLValueStatement FListValueStatement
  | FTValueStatement FTupleValueStatement
  | FAValueStatement FFunApplication
  | FIValueStatement Int
  | FLitStrValueStatement String
  | FFValueStatement String FValueStatement
  | FExpr FValueStatement FValueStatementExpr
  deriving (Show)

data FListValueStatement = FListValueStatement [FValueStatement]
  deriving (Show)

data FTupleValueStatement = FTupleValueStatement [FValueStatement]
  deriving (Show)

data FFunApplication =
    FSFunApplication String FFunApplication
  | FFunApplicationB String [FFunctionArgAppl]
  deriving (Show)

data FFunctionARgAppl
