module StaticCheck.StaticCheck where
import StaticCheck.Format
import StaticCheck.InitialConvertToProgramFormat (initialConvertToProgramFormat)
import StaticCheck.NConvert (convertToNPF)
import StaticCheck.CheckProgramFormat (checkProgramFormat)
import Bnfc.AbsHpl
import Bnfc.ErrM

staticCheck :: Program (Maybe (Int, Int)) -> Err (NProgramFormat, [String])
staticCheck p = do
  let pf = initialConvertToProgramFormat p
  (cpf, warnings) <- checkProgramFormat pf
  let npf = convertToNPF cpf
  return (npf, warnings)