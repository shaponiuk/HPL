module StaticCheck.Format where
  
import Data.Map
import Debug.Trace

data E = E {
    names :: Map String [Int]
} deriving (Show)

data S = S {
  vars :: Map Int (FType, FValueStatement),
  newInt :: Int,
  functionArgs :: Map Int [FPatternMatch]
} deriving (Show)

getNewLoc :: S -> (Int, S)
getNewLoc (S vars loc funArgs) = (loc + 1, S vars (loc + 1) funArgs)

putInLoc :: Int -> (FType, FValueStatement) -> S -> S
putInLoc loc thing (S vars newInt funArgs) =
  S (insert loc thing vars) newInt funArgs

stateLookup :: Int -> S -> (FType, FValueStatement)
stateLookup loc (S varsMap _ _) = trace ("here7") $ varsMap ! loc

registerLoc :: E -> String -> Int -> E
registerLoc (E names) name loc =
  if member name names 
    then
      let
        locs = trace ("here10: " ++ name) $ names ! name
      in E (insert name (loc:locs) names)
    else 
      E (insert name [loc] names)

lookupLoc :: String -> E -> [Int]
lookupLoc name (E names) = trace ("here8: " ++ name ++ (show names)) $ if member name names then names ! name else []

lookupFirstLoc :: String -> E -> Int
lookupFirstLoc name env = head $ lookupLoc name env

funArgNamesLookup :: S -> Int -> [FPatternMatch]
funArgNamesLookup (S _ _ funArgs) loc =
  if member loc funArgs
    then trace "here9" $ funArgs ! loc
    else []

putArgNames :: S -> Int -> [FPatternMatch] -> S
putArgNames (S vars newInt functionArgs) loc strs = 
  S vars newInt (insert loc strs functionArgs)

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
  
data NFNonSusFunDef = NFNonSusFunDef String [FPatternMatch] E
  deriving (Show)

type FunRunT = S -> [FValueStatement] -> (IO (Maybe (S, FValueStatement)))

type FunRunQuickT = S -> IO (Maybe (S, FValueStatement))

data NFSusFunDef = TODO1
  deriving (Show)

data NFRefDef = TODO2
  deriving (Show)

data ProgramFormat = SITList [FStruct] [FInterface] [FAlgType]
  deriving (Eq,Ord,Show)

data FStruct = FStructB String FStructBody | FStructI String [String] FStructBody
  deriving (Eq,Ord,Show)

convertStringToPM :: String -> FPatternMatch
convertStringToPM str = FPatternMatchB str

convertStringsToPMs :: [String] -> [FPatternMatch]
convertStringsToPMs = Prelude.map convertStringToPM

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
  | FExpr FValueStatementExpr
  deriving (Eq,Ord,Show)

data FFunApplication =
    FSFunApplication String FFunApplication
  | FFunApplicationB String [FValueStatement]
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