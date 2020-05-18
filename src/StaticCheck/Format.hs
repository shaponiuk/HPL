module StaticCheck.Format where

import Data.Map
import Util.Util

data QueueT = QueueT {
  env :: E,
  valueStatement :: FValueStatement, 
  queueId :: Int, 
  finished :: Bool, 
  yielding :: Bool
} deriving (Show)

data SemaphoreT = SemaphoreT {
  waitingQueues :: [Int],
  semaphoreValue :: Int,
  semaphoreId :: Int
} deriving (Show)

  -- if function (bool in vars)
data S = S {
  vars :: Map Int (E, FValueStatement),
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
  deriving (Show)

convertStringToPM :: String -> FPatternMatch
convertStringToPM = FPatternMatchB Nothing

convertStringsToPMs :: [String] -> [FPatternMatch]
convertStringsToPMs = Prelude.map convertStringToPM

data FAlgType = FAlgType (Maybe (Int, Int)) String [String] [FAlgTypeVal]
  deriving (Show)

data FFunctionDef =
    NonSusFFunctionDef (Maybe (Int, Int)) FType String [FPatternMatch] FValueStatement
  | SusFFunctionDef FFunctionDef
  deriving (Show)

data FRefDef = FRefDef (Maybe (Int, Int)) FType String FValueStatement
  deriving (Show)

data FPatternMatch =
    FPatternMatchI (Maybe (Int, Int)) Int
  | FPatternMatchB (Maybe (Int, Int)) String
  | FPatternMatchT (Maybe (Int, Int)) [FPatternMatch]
  | FPatternMatchC (Maybe (Int, Int)) FPatternMatch [FPatternMatch]
  deriving (Show)

data FType =
    FTypeB (Maybe (Int, Int)) String [FType]
  | FunFType (Maybe (Int, Int)) FType FType
  | FTypeT (Maybe (Int, Int)) [FType]
  deriving (Show)

data FValueStatement =
    FValueStatementB (Maybe (Int, Int)) [FAssignment] FValueStatement
  | FForceValueStatement (Maybe (Int, Int)) [FAssignment] FValueStatement
  | FIfValueStatement (Maybe (Int, Int)) FValueStatement FValueStatement FValueStatement
  | FLValueStatement (Maybe (Int, Int)) [FValueStatement]
  | FTValueStatement (Maybe (Int, Int)) [FValueStatement]
  | FAValueStatement (Maybe (Int, Int)) FFunApplication
  | FIValueStatement (Maybe (Int, Int)) Int
  | FLitStrValueStatement (Maybe (Int, Int)) String
  | FFValueStatement (Maybe (Int, Int)) String FValueStatement
  | FCValueStatement (Maybe (Int, Int)) String [FValueStatement]
  | FExpr (Maybe (Int, Int)) FValueStatementExpr
  | FRefAddr Int
  | FSusValueStatement FValueStatement
  | FSuspendedValue Int
  | FSemaphore Int
  | FNTValueStatement Int FValueStatement

data FFunApplication =
    FSFunApplication (Maybe (Int, Int)) String FFunApplication
  | FFunApplicationB (Maybe (Int, Int)) String [FValueStatement]
  | FFunApplicationR Int
  deriving (Show)

data FAssignment =
    FAssignmentB (Maybe (Int, Int)) FType FPatternMatch FValueStatement
  | FRefAssignment (Maybe (Int, Int)) FRefDef
  deriving (Show)

data FAlgTypeVal = FAlgTypeVal (Maybe (Int, Int)) String FType
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

instance Show FValueStatement where
  show (FLitStrValueStatement _ str) = str
  show (FIValueStatement _ i) = show i
  show (FTValueStatement _ l) = showTupleList l
  show (FCValueStatement _ constructorName args) = constructorName ++ " " ++ showTupleList args
  show (FExpr _ expr) = show expr
  show FValueStatementB{} = "let statement"
  show FForceValueStatement{} = "force let statement"

instance Show FValueStatementExpr where
  show (FEAdd vs1 vs2) = show vs1 ++ " + " ++ show vs2

showTupleList [] = "()"
showTupleList [x] = "(" ++ show x ++ ")"
showTupleList (x:x2:xs) =
  let
    '(':rest = showTupleList (x2:xs)
  in "(" ++ show x ++ ", " ++ rest

getFunctionName :: FFunctionDef -> String
getFunctionName (NonSusFFunctionDef _ _ name _ _) = name
getFunctionName (SusFFunctionDef fd) = getFunctionName fd

instance Eq FValueStatement where
  vs1 == vs2 = traceD (show vs1 ++ show vs2) undefined

instance Eq FType where
  (FTypeT _ l1) == (FTypeT _ l2) = l1 == l2
  t1 == t2 = traceD (show t1 ++ show t2) undefined