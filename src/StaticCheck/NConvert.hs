module StaticCheck.NConvert where

import Data.Map
import StaticCheck.Format

convertToNPF :: ISATProgramFormat -> NProgramFormat
convertToNPF (ISATSIT structMap interfaceMap algTypeMap) =
    NSIT nfStructMap interfaceMap algTypeMap where
    nfStructMap = map fstructConvert structMap

fstructConvert :: FStruct -> NFStruct
fstructConvert (FStructI name (FStructBody body)) =
    NFStruct name []
        (convertPrivateNonSusFuns name body)
        (convertPublicNonSusFuns name body)
        TODO1
        TODO1
        TODO2
        TODO2

convertPrivateNonSusFuns :: String -> [FStructField] -> [NFNonSusFunDef] =
    let env = 