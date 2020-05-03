module StaticCheck.StaticCheck where
import StaticCheck.Format
import StaticCheck.InitialConvertToProgramFormat (initialConvertToProgramFormat)
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
  let npf = convertToNPF pf
  return npf