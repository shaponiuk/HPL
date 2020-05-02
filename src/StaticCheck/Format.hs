module StaticCheck.Format where

import Data.Map

  -- if function (bool in vars)
data S = S {
  vars :: Map Int (Bool, E, FType, FValueStatement),
  newInt :: Int,
  functionArgs :: Map Int [FPatternMatch]
} deriving (Show)

newtype E = E {
    names :: Map String [Int]
} deriving (Show)

data NProgramFormat = NSIT [NFStruct] S
  deriving (Show)

data NFStruct = NFStruct String NFStructBody
  deriving (Show)

-- private,public,private,public,private,public
data NFStructBody = 
  NFStructBody 
    [NFNonSusFunDef] 
    [NFNonSusFunDef] 
    [NFSusFunDef] 
    [NFSusFunDef] 
    [NFRefDef] 
    [NFRefDef]
    deriving (Show)
  
data NFNonSusFunDef = NFNonSusFunDef String [FPatternMatch] E
  deriving (Show)

type FunRunT = S -> [FValueStatement] -> IO (Maybe (S, FValueStatement))

type FunRunQuickT = S -> IO (Maybe (S, FValueStatement))

data NFSusFunDef = TODO1
  deriving (Show)

data NFRefDef = TODO2
  deriving (Show)

data ProgramFormat = SITList [FStruct] [FAlgType]
  deriving (Eq,Ord,Show)

data FStruct = FStructB String [FStructField] | FStructI String [String] [FStructField]
  deriving (Eq,Ord,Show)

convertStringToPM :: String -> FPatternMatch
convertStringToPM = FPatternMatchB

convertStringsToPMs :: [String] -> [FPatternMatch]
convertStringsToPMs = Prelude.map convertStringToPM

data FAlgType = FAlgType String [String] [FAlgTypeVal]
  deriving (Eq,Ord,Show)

data FStructField = 
    FStructFieldFunPrivate FFunctionDef
  | FStructFieldFunPublic FFunctionDef
  | FStructFieldRefPrivate FRefDef
  | FStructFieldRefPublic FRefDef
  deriving (Eq,Ord,Show)

data FFunctionDef =
    NonSusFFunctionDef FType String [FPatternMatch] FValueStatement
  | SusFFunctionDef FFunctionDef
  deriving (Eq,Ord,Show)

data FRefDef = FRefDef FType String FValueStatement
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
  | FCValueStatement String [FValueStatement]
  | FExpr FValueStatementExpr
  | FRefAddr Int
  deriving (Eq,Ord,Show)

data FFunApplication =
    FSFunApplication String FFunApplication
  | FFunApplicationB String [FValueStatement]
  | FFunApplicationR [Int] [FValueStatement]
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