module StaticCheck.Format where
  
import Data.Map

data S = S {
  vars :: Map String (Type, FValueStatement),
  runQueue :: [RunFunT]
}

data NProgramFormat = NSIT (Map String NFStruct) (Map String FInterface) (Map String FAlgType)
  deriving (Eq,Ord,Show)

data NFStruct = NFStruct String [String] NFStructBody
  deriving (Eq,Ord,Show)

-- private,public,private,public,private,public
data NFStructBody = 
  NFStructBody 
    [NFNonSusFunDef] 
    [NFNonSusFunDef] 
    [NFSusFunDef] 
    [NFSusFunDef] 
    [NFRefDef] 
    [NFRefDef]
  
data NFNonSusFunDef = NFNonSusFunDef FunRunT

type FunRunT = S -> [FValueStatement] -> IO (S)

data NFSusFunDef = TODO1

data NFRefDef = TODO2

data ProgramFormat = SITList [FStruct] [FInterface] [FAlgType]
  deriving (Eq,Ord,Show)

data ATProgramFormat = ATSIT [FStruct] [FInterface] (Map String FAlgType)
  deriving (Eq,Ord,Show)

data SATProgramFormat = SATSIT (Map String FStruct) [FInterface] (Map String FAlgType)
  deriving (Eq,Ord,Show)

data ISATProgramFormat = ISATSIT (Map String FStruct) (Map String FInterface) (Map String FAlgType)
  deriving (Eq,Ord,Show)

data FStruct = FStructB String FStructBody | FStructI String [String] FStructBody
  deriving (Eq,Ord,Show)

data FInterface = 
    FInterfaceB String FInterfaceBody 
  | FInterfaceI String [String] FInterfaceBody
  deriving (Eq,Ord,Show)
 
data FAlgType = FAlgType String [String] [FAlgTypeVal]
  deriving (Eq,Ord,Show)

data FStructBody = FStructBody [FStructField]
  deriving (Eq,Ord,Show)

data FStructField = 
    FStructFieldFunPrivate FFunctionDef
  | FStructFieldFunPublic FFunctionDef
  | FStructFieldRefPrivate FRefDef
  | FStructFieldRefPublic FRefDef
  deriving (Eq,Ord,Show)

data FFunctionDef =
    NonSusFFunctionDef FType String [FFunctionArg] FValueStatement
  | SusFFunctionDef FFunctionDef
  deriving (Eq,Ord,Show)

data FRefDef = FRefDef FType String FValueStatement
  deriving (Eq,Ord,Show)

data FFunctionArg = FFunctionArg FPatternMatch
  deriving (Eq,Ord,Show)

data FPatternMatch =
    FPatternMatchI Int
  | FPatternMatchB String
  | FPatternMatchT [FPatternMatch]
  | FPatternMatchC FPatternMatch [FPatternMatch]
  deriving (Eq,Ord,Show)

data FType =
    FTypeB String [FType]
  | FunFType FType FType
  | FTypeT [FType]
  deriving (Eq,Ord,Show)

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
  deriving (Eq,Ord,Show)

data FFunApplication =
    FSFunApplication String FFunApplication
  | FFunApplicationB String [FFunctionArgAppl]
  deriving (Eq,Ord,Show)

data FFunctionArgAppl = FFunctionArgAppl FValueStatement
  deriving (Eq,Ord,Show)

data FInterfaceBody = FInterfaceBody [FFunOrRefDecl]
  deriving (Eq,Ord,Show)

data FFunOrRefDecl =
    FFunOrRefDeclF FType String
  | FFunOrRefDeclSF FType String
  | FFunOrRefDeclR FType String
  deriving (Eq,Ord,Show)

data FAssignment =
    FAssignmentB FType FPatternMatch FValueStatement
  | FRefAssignment FRefDef
  deriving (Eq,Ord,Show)

data FAlgTypeVal = FAlgTypeVal String FType
  deriving (Eq,Ord,Show)

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
  deriving (Eq,Ord,Show)