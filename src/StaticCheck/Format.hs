module StaticCheck.Format where
  
import Data.Map

data E = E {
    names :: Map String [Int]
} deriving (Show)

data S = S {
  vars :: Map Int (FType, E, FValueStatement),
  newInt :: Int
} deriving (Show)

getNewLoc :: S -> (Int, S)
getNewLoc (S vars loc) = (loc + 1, S vars (loc + 1))

putInLoc :: Int -> (FType, E, FValueStatement) -> S -> S
putInLoc loc thing (S vars newInt) =
  S (insert loc thing vars) newInt

registerLoc :: E -> String -> Int -> E
registerLoc (E names) name loc =
  if member name names 
    then
      let
        locs = names ! name
      in E (insert name (loc:locs) names)
    else 
      E (insert name [loc] names)

lookupLoc :: String -> E -> [Int]
lookupLoc name (E names) = names ! name

lookupFirstLoc :: String -> E -> Int
lookupFirstLoc name env = head $ lookupLoc name env

data NProgramFormat = NSIT [NFStruct] [FInterface] [FAlgType] S
  deriving (Show)

data NFStruct = NFStruct String [String] NFStructBody
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
  
data NFNonSusFunDef = NFNonSusFunDef String [FFunctionArg]
  deriving (Show)

type FunRunT = S -> [FValueStatement] -> Maybe (IO ((S, FValueStatement)))

type FunRunQuickT = S -> Maybe (IO ((S, FValueStatement)))

data NFSusFunDef = TODO1
  deriving (Show)

data NFRefDef = TODO2
  deriving (Show)

data ProgramFormat = SITList [FStruct] [FInterface] [FAlgType]
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