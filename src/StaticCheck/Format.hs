module StaticCheck.Format where

newtype IdentF = IdentF String
  deriving (Show)

data ProgramFormat = SITList [SIT]
  deriving (Show)

data SIT = SITStruct FStruct | SITInterface FInterface | SITType FAlgType
  deriving (Show)

data FStruct = FStructB IdentF [IdentF] FStructBody
  deriving (Show)

data FInterface = FInterface
  deriving (Show)
 
data FAlgType = FAlgType
  deriving (Show)

data FStructBody = FStructBody [FStructField]
  deriving (Show)

data FStructField = FStructField
