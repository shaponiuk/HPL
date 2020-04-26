module StaticCheck.StaticCheck where

import StaticCheck.Format
import StaticCheck.InitialConvertToProgramFormat (initialConvertToProgramFormat)
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
  at <- checkAlgTypes pf
  fail $ show at
  return at

