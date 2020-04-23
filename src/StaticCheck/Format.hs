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

