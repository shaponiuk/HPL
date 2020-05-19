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
  show FIfValueStatement{} = "if statement"
  show (FAValueStatement _ (FFunApplicationB _ funName vss)) = "function application: " ++ funName ++ " with args: " ++ show vss
  show (FAValueStatement _ (FFunApplicationR loc)) = "function application located at: " ++ show loc
  show FFValueStatement{} = "lambda statement"
  show (FRefAddr addr) = "reference at: " ++ show addr
  show FSusValueStatement{} = "suspended value statement"
  show FSuspendedValue{} = "suspended value"
  show FSemaphore{} = "semaphore"
  show FNTValueStatement{} = "tuple element value"

instance Show FValueStatementExpr where
  show (FEAdd vs1 vs2) = show vs1 ++ " + " ++ show vs2
  show (FESub vs1 vs2) = show vs1 ++ " - " ++ show vs2
  show (FEDiv vs1 vs2) = show vs1 ++ " / " ++ show vs2
  show (FEMul vs1 vs2) = show vs1 ++ " * " ++ show vs2
  show (FEEQ vs1 vs2) = show vs1 ++ " == " ++ show vs2

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
  (FAValueStatement _ (FFunApplicationB _ funName1 vss1)) == (FAValueStatement _ (FFunApplicationB _ funName2 vss2)) = 
    funName1 == funName2 && vss1 == vss2
  (FIValueStatement _ i1) == (FIValueStatement _ i2) = i1 == i2
  (FExpr _ expr1) == (FExpr _ expr2) = expr1 == expr2

instance Eq FValueStatementExpr where
  (FEAdd vs11 vs12) == (FEAdd vs21 vs22) = vs11 == vs21 && vs12 == vs22
  (FESub vs11 vs12) == (FESub vs21 vs22) = vs11 == vs21 && vs12 == vs22
  (FEMul vs11 vs12) == (FEMul vs21 vs22) = vs11 == vs21 && vs12 == vs22
  (FEDiv vs11 vs12) == (FEDiv vs21 vs22) = vs11 == vs21 && vs12 == vs22

instance Eq FType where
  (FTypeT _ l1) == (FTypeT _ l2) = l1 == l2
  (FTypeB _ tName1 tArgs1) == (FTypeB _ tName2 tArgs2) = tName1 == tName2 && tArgs1 == tArgs2
  (FunFType _ t11 t12) == (FunFType _ t21 t22) = t11 == t21 && t12 == t22