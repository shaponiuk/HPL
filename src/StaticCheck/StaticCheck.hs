module StaticCheck.StaticCheck where

import StaticCheck.Format
import StaticCheck.InitialConvertToProgramFormat (initialConvertToProgramFormat)
import StaticCheck.CheckAlgTypes (checkAlgTypes)
import StaticCheck.CheckStructs (checkStructs)
import StaticCheck.CheckInterfaces (checkInterfaces)
import StaticCheck.NConvert (convertToNPF)
import Bnfc.AbsHpl
import Bnfc.ErrM

staticCheck :: Program -> Err NProgramFormat
staticCheck p = staticCheckErrPack $ staticCheckEither p

staticCheckErrPack :: Either String a -> Err a
staticCheckErrPack (Left e) = Bad e
staticCheckErrPack (Right r) = Ok r

staticCheckEither :: Program -> Either String NProgramFormat
staticCheckEither p = do
  let pf = initialConvertToProgramFormat p
  at <- checkAlgTypes pf
  sat <- checkStructs at
  isat <- checkInterfaces sat
  let npf = convertToNPF isat
  return npf

