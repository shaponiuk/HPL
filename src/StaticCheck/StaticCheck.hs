module StaticCheck.StaticCheck where

import StaticCheck.Format
import Bnfc.AbsHpl
import Bnfc.ErrM

staticCheck :: Program -> Err ProgramFormat
staticCheck p = staticCheckErrPack $ staticCheckEither p

staticCheckErrPack :: Either String ProgramFormat -> Err ProgramFormat
staticCheckErrPack (Left e) = Bad e
staticCheckErrPack (Right r) = Ok r

staticCheckEither :: Program -> Either String ProgramFormat
staticCheckEither p = do
  let pf = initialConvertToProgramFormat p
  fail $ show pf
  return pf

initialConvertToProgramFormat :: Program -> ProgramFormat
initialConvertToProgramFormat (ProgramB l) = SITList $ map absSITToSit l

absSITToSit :: StructOrInterfaceOrType -> SIT
absSITToSit (StructOrInterfaceOrTypeS s) = SITStruct $ convertStruct s
absSITToSit (StructOrInterfaceOrTypeI i) = undefined
absSITToSit (StructOrInterfaceOrTypeT t) = undefined

convertStruct :: Struct -> FStruct
convertStruct (StructB (Ident name) body) = 
  FStructB name $ convertBody body
convertStruct (StructI (Ident name) interfaceIdList body) = 
  FStructI name (convertInterfaceIdList interfaceIdList) $ convertBody body

convertInterfaceIdList :: [InterfaceId] -> [String]
convertInterfaceIdList list = convertIdentList $ map unwrapInterfaceId list

unwrapInterfaceId :: InterfaceId -> Ident
unwrapInterfaceId (InterfaceIdB ident) = ident

convertIdentList :: [Ident] -> [String]
convertIdentList [] = []
convertIdentList ((Ident str):xs) = str:(convertIdentList xs)

convertBody :: StructBody -> FStructBody
convertBody (StructBodyB structFieldList) = 
  FStructBody $ map convertStructField structFieldList

convertStructField :: StructField -> FStructField
convertStructField (StructFieldFunPr fd) = FStructFieldFunPrivate $ convertFunctionDef fd
convertStructField (StructFieldFunPu fd) = FStructFieldFunPublic $ convertFunctionDef fd
convertStructField (StructFieldRefPr ref) = FStructFieldRefPrivate $ convertRefDef ref
convertStructField (StructFieldRefPu ref) = FStructFieldRefPublic $ convertRefDef ref

convertFunctionDef = undefined

convertRefDef = undefined

setArithmetics = undefined
