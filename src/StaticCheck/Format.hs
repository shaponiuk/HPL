module StaticCheck.Format where

import Data.Map
import Util.Util

data QueueT = QueueT {
  env :: E,
  valueStatement :: FValueStatement, 
  queueId :: Int, 
  finished :: Bool, 
  yielding :: Bool,
  printQ :: [String],
  getLineQ :: [String]
} deriving (Show)

data SemaphoreT = SemaphoreT {
  waitingQueues :: [Int],
  semaphoreValue :: Int,
  semaphoreId :: Int
} deriving (Show)

  -- if function (bool in vars)
data S = S {
  vars :: Map Int (Bool, E, FType, FValueStatement),
  newInt :: Int,
  functionArgs :: Map Int [FPatternMatch],
  semaphores :: [SemaphoreT],
  queues :: [QueueT]
} deriving (Show)

newtype E = E {
    names :: Map String [Int]
} deriving (Show)

data NProgramFormat = NSIT E S
  deriving (Show)

data ProgramFormat = SITList [FFunctionDef] [FRefDef] [FAlgType]
  deriving (Eq,Ord,Show)

convertStringToPM :: String -> FPatternMatch
convertStringToPM = FPatternMatchB

convertStringsToPMs :: [String] -> [FPatternMatch]
convertStringsToPMs = Prelude.map convertStringToPM

data FAlgType = FAlgType String [String] [FAlgTypeVal]
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
  | FPatternMatchS Int -- should be put in loc directly
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
  | FSusValueStatement FValueStatement
  | FSuspendedValue Int
  | FSemaphore Int
  | FNTValueStatement Int FValueStatement
  deriving (Eq,Ord,Show)

data FFunApplication =
    FSFunApplication String FFunApplication
  | FFunApplicationB String [FValueStatement]
  | FFunApplicationR Int
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
  deriving (Eq,Ord, Show)

-- instance Show FValueStatement where
  -- show (FLitStrValueStatement str) = str
  -- show (FIValueStatement i) = show i
  -- show (FTValueStatement l) = showTupleList l
  -- show (FCValueStatement constructorName args) = constructorName ++ " " ++ show args
  -- show a = "???????? heeeerrreeeee ?????????"

showTupleList [] = "()"
showTupleList [x] = "(" ++ show x ++ ")"
showTupleList (x:x2:xs) =
  let
    '(':rest = showTupleList (x2:xs)
  in "(" ++ show x ++ ", " ++ rest

getFunctionName :: FFunctionDef -> String
getFunctionName (NonSusFFunctionDef _ name _ _) = name
getFunctionName (SusFFunctionDef fd) = getFunctionName fd