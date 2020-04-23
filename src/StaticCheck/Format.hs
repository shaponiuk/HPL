module StaticCheck.Format where

newtype IdentF = IdentF String
  deriving (Show)

data ProgramFormat = SITList [SIT]
  deriving (Show)

data SIT = SITStruct FStruct | SITInterface FInterface | SITType FAlgType
  deriving (Show)

data FStruct = FStructB Ident [FInterfaceId] FStructBody
  deriving (Show)

data FInterface = FInterface
  deriving (Show)
 
data FAlgType = FAlgType
  deriving (Show)
