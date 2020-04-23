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
absSITToSit (StructOrInterfaceOrTypeS s) = SITStruct FStruct

setArithmetics = undefined
