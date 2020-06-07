module StaticCheck.Format where

import Data.Map
import Util.Util
import Data.Char (ord, chr)

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

data FType =
    FTypeB (Maybe (Int, Int)) String [FType]
  | FunFType (Maybe (Int, Int)) FType FType
  | FTypeT (Maybe (Int, Int)) [FType]

data FValueStatement =
    FValueStatementB (Maybe (Int, Int)) [FAssignment] FValueStatement
  | FForceValueStatement (Maybe (Int, Int)) [FAssignment] FValueStatement
  | FIfValueStatement (Maybe (Int, Int)) FValueStatement FValueStatement FValueStatement
  | FTValueStatement (Maybe (Int, Int)) [FValueStatement]
  | FAValueStatement (Maybe (Int, Int)) FFunApplication
  | FIValueStatement (Maybe (Int, Int)) Int
  | FFValueStatement (Maybe (Int, Int)) String FValueStatement
  | FCValueStatement (Maybe (Int, Int)) String [FValueStatement]
  | FExpr (Maybe (Int, Int)) FValueStatementExpr
  | FRefAddr Int
  | FSusValueStatement FValueStatement
  | FSuspendedValue Int
  | FSemaphore Int
  | FNTValueStatement Int FValueStatement

data FFunApplication =
  FFunApplicationB (Maybe (Int, Int)) String [FValueStatement]
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
  show (FIValueStatement _ i) = show i
  show (FTValueStatement _ l) = showTupleList l
  show (FCValueStatement _ "SEmptyListC" []) = ""
  show (FCValueStatement _ "SListC" [FIValueStatement _ i, l]) = chr i : show l
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
  show (FNTValueStatement n vs) = "tuple element value - " ++ show n ++ " " ++ show vs

instance Show FValueStatementExpr where
  show (FEAdd vs1 vs2) = show vs1 ++ " + " ++ show vs2
  show (FESub vs1 vs2) = show vs1 ++ " - " ++ show vs2
  show (FEDiv vs1 vs2) = show vs1 ++ " / " ++ show vs2
  show (FEMul vs1 vs2) = show vs1 ++ " * " ++ show vs2
  show (FEMod vs1 vs2) = show vs1 ++ " mod " ++ show vs2
  show (FEL vs1 vs2) = show vs1 ++ " < " ++ show vs2
  show (FELQ vs1 vs2) = show vs1 ++ " <= " ++ show vs2
  show (FEG vs1 vs2) = show vs1 ++ " > " ++ show vs2
  show (FEGQ vs1 vs2) = show vs1 ++ " >= " ++ show vs2
  show (FEEQ vs1 vs2) = show vs1 ++ " == " ++ show vs2
  show (FENE vs1 vs2) = show vs1 ++ " != " ++ show vs2

showTupleList [] = "()"
showTupleList [x] = "(" ++ show x ++ ")"
showTupleList (x:x2:xs) =
  let
    '(':rest = showTupleList (x2:xs)
  in "(" ++ show x ++ ", " ++ rest

getFunctionName :: FFunctionDef -> String
getFunctionName (NonSusFFunctionDef _ _ name _ _) = name
getFunctionName (SusFFunctionDef fd) = getFunctionName fd

instance Show FType where
  show (FTypeB _ name args) = name ++ " " ++ showTupleList args
  show (FunFType _ t1 t2) = "(" ++ show t1 ++ " -> " ++ show t2 ++ ")"
  show (FTypeT _ ts) = showTupleList ts

instance Show FPatternMatch where
  show (FPatternMatchB _ x) = x
  show (FPatternMatchI _ i) = show i
  show (FPatternMatchT _ pms) = showTupleList pms
  show (FPatternMatchC _ name args) = show name ++ " " ++ showTupleList args

instance Eq FPatternMatch where
  (FPatternMatchI _ i1) == (FPatternMatchI _ i2) = i1 == i2
  FPatternMatchI{} == _ = False
  (FPatternMatchB _ x1) == (FPatternMatchB _ x2) = x1 == x2
  FPatternMatchB{} == _ = False
  (FPatternMatchT _ t1) == (FPatternMatchT _ t2) = t1 == t2
  FPatternMatchT{} == _ = False
  (FPatternMatchC _ c1 args1) == (FPatternMatchC _ c2 args2) = c1 == c2 && args1 == args2
  FPatternMatchC{} == _ = False

instance Eq FRefDef where
  (FRefDef _ t1 name1 vs1) == (FRefDef _ t2 name2 vs2) = t1 == t2 && name1 == name2 && vs1 == vs2

instance Eq FAssignment where
  (FAssignmentB _ t1 pm1 vs1) == (FAssignmentB _ t2 pm2 vs2) = t1 == t2 && pm1 == pm2 && vs1 == vs2
  FAssignmentB{} == _ = False
  (FRefAssignment _ rd1) == (FRefAssignment _ rd2) = rd1 == rd2
  FRefAssignment{} == _ = False

instance Eq FFunApplication where
  (FFunApplicationB _ name1 vss1) == (FFunApplicationB _ name2 vss2) = name1 == name2 && vss1 == vss2
  FFunApplicationB{} == _ = False
  (FFunApplicationR loc1) == (FFunApplicationR loc2) = loc1 == loc2
  FFunApplicationR{} == _ = False

instance Eq FValueStatement where
  (FAValueStatement _ f1) == (FAValueStatement _ f2) = f1 == f2
  FAValueStatement{} == _ = False
  (FIValueStatement _ i1) == (FIValueStatement _ i2) = i1 == i2
  FIValueStatement{} == _ = False
  (FExpr _ expr1) == (FExpr _ expr2) = expr1 == expr2
  FExpr{} == _ = False
  (FValueStatementB _ assignments1 vs1) == (FValueStatementB _ assignments2 vs2) = assignments1 == assignments2 && vs1 == vs2
  FValueStatementB{} == _ = False
  (FForceValueStatement _ assignments1 vs1) == (FForceValueStatement _ assignments2 vs2) = assignments1 == assignments2 && vs1 == vs2
  FForceValueStatement{} == _ = False
  (FIfValueStatement _ c1 vs11 vs12) == (FIfValueStatement _ c2 vs21 vs22) = c1 == c2 && vs11 == vs21 && vs12 == vs22
  FIfValueStatement{} == _ = False
  (FTValueStatement _ ts1) == (FTValueStatement _ ts2) = ts1 == ts2
  FTValueStatement{} == _ = False
  (FFValueStatement _ arg1 vs1) == (FFValueStatement _ arg2 vs2) = arg1 == arg2 && vs1 == vs2
  FFValueStatement{} == _ = False
  (FCValueStatement _ pm1 args1) == (FCValueStatement _ pm2 args2) = pm1 == pm2 && args1 == args2
  FCValueStatement{} == _ = False
  (FRefAddr a1) == (FRefAddr a2) = a1 == a2
  FRefAddr{} == _ = False
  (FSusValueStatement vs1) == (FSusValueStatement vs2) = vs1 == vs2
  FSusValueStatement{} == _ = False
  (FSuspendedValue q1) == (FSuspendedValue q2) = q1 == q2
  FSuspendedValue{} == _ = False
  (FSemaphore id1) == (FSemaphore id2) = id1 == id2
  FSemaphore{} == _ = False
  (FNTValueStatement n1 vs1) == (FNTValueStatement n2 vs2) = n1 == n2 && vs1 == vs2
  FNTValueStatement{} == _ = False

instance Eq FValueStatementExpr where
  (FEAdd vs11 vs12) == (FEAdd vs21 vs22) = vs11 == vs21 && vs12 == vs22
  FEAdd{} == _ = False
  (FESub vs11 vs12) == (FESub vs21 vs22) = vs11 == vs21 && vs12 == vs22
  FESub{} == _ = False
  (FEMul vs11 vs12) == (FEMul vs21 vs22) = vs11 == vs21 && vs12 == vs22
  FEMul{} == _ = False
  (FEDiv vs11 vs12) == (FEDiv vs21 vs22) = vs11 == vs21 && vs12 == vs22
  FEDiv{} == _ = False
  (FEMod vs11 vs12) == (FEMod vs21 vs22) = vs11 == vs21 && vs12 == vs22
  FEMod{} == _ = False
  (FEL vs11 vs12) == (FEL vs21 vs22) = vs11 == vs21 && vs12 == vs22
  FEL{} == _ = False
  (FELQ vs11 vs12) == (FELQ vs21 vs22) = vs11 == vs21 && vs12 == vs22
  FELQ{} == _ = False
  (FEG vs11 vs12) == (FEG vs21 vs22) = vs11 == vs21 && vs12 == vs22
  FEG{} == _ = False
  (FEGQ vs11 vs12) == (FEGQ vs21 vs22) = vs11 == vs21 && vs12 == vs22
  FEGQ{} == _ = False
  (FEEQ vs11 vs12) == (FEEQ vs21 vs22) = vs11 == vs21 && vs12 == vs22
  FEEQ{} == _ = False
  (FENE vs11 vs12) == (FENE vs21 vs22) = vs11 == vs21 && vs12 == vs22
  FENE{} == _ = False

instance Eq FType where
  (FTypeT _ [t1]) == t2 = t1 == t2
  t1 == (FTypeT _ [t2]) = t1 == t2
  (FTypeT _ l1) == (FTypeT _ l2) = l1 == l2
  (FTypeB _ tName1 tArgs1) == (FTypeB _ tName2 tArgs2) = tName1 == tName2 && tArgs1 == tArgs2
  (FunFType _ t11 t12) == (FunFType _ t21 t22) = t11 == t21 && t12 == t22
  FTypeB{} == FunFType{} = False
  FTypeB{} == FTypeT{} = False
  FunFType{} == FTypeB{} = False
  FunFType{} == FTypeT{} = False
  FTypeT{} == FTypeB{} = False
  FTypeT{} == FunFType{} = False


convertString :: Maybe (Int, Int) -> String -> FValueStatement
convertString pos [] = FCValueStatement pos "SEmptyListC" []
convertString pos (x:xs) =
  let
    l = convertString pos xs
    i = ord x
  in FCValueStatement pos "SListC" [FIValueStatement pos i, l]

getVSLoc :: FValueStatement -> Maybe (Int, Int)
getVSLoc (FAValueStatement loc _) = loc
getVSLoc (FValueStatementB loc _ _) = loc
getVSLoc (FForceValueStatement loc _ _) = loc
getVSLoc (FIfValueStatement loc _ _ _) = loc
getVSLoc (FTValueStatement loc _) = loc
getVSLoc (FIValueStatement loc _) = loc
getVSLoc (FFValueStatement loc _ _) = loc
getVSLoc (FCValueStatement loc _ _) = loc
getVSLoc (FExpr loc _) = loc
getVSLoc FRefAddr{} = Nothing
getVSLoc FSusValueStatement{} = Nothing
getVSLoc FSuspendedValue{} = Nothing
getVSLoc FSemaphore{} = Nothing
getVSLoc FNTValueStatement{} = Nothing