module StaticCheck.NConvert where

import Data.Map
import StaticCheck.Format

convertToNPF :: ISATProgramFormat -> NProgramFormat
convertToNPF (ISATSIT structMap interfaceMap algTypeMap) =
    NSIT nfStructMap interfaceMap algTypeMap state where
    (nfStructMap, state) = getStates $ Data.Map.map fstructConvert structMap

getStates :: Map String (NFStruct, S) -> (Map String NFStruct, S)
getStates = undefined

fstructConvert :: FStruct -> (NFStruct, S)
fstructConvert (FStructB name (FStructBody body)) =
    (NFStruct name [] (
        NFStructBody
        []
        publicNonSus
        []
        []
        []
        []),
    state) where
    (publicNonSus, state) = convertPublicNonSusFuns name body

convertPublicNonSusFuns :: String -> [FStructField] -> ([NFNonSusFunDef], S)
convertPublicNonSusFuns name fields =
    let env = makeStructEnv name fields
    in (Prelude.map (convertFStructFieldPublic env) fields, registerStates env name fields)

convertFStructFieldPublic :: E -> FStructField -> NFNonSusFunDef
convertFStructFieldPublic env (FStructFieldFunPublic (NonSusFFunctionDef typ name args vs)) =
    let
        newEnv = registerArgs args env
    in let 
        f = (\state argVs ->
            let newState = putInState args argVs newEnv state
            in runValueStatement newEnv newState vs
            )
    in NFNonSusFunDef f

makeStructEnv :: String -> [FStructField] -> S -> (E, S)
makeStructEnv fStructName fields state =
    Prelude.foldl registerField (E empty, state) fields

registerField :: (E, S) -> FStructField -> (E, S)
registerField = undefined

runValueStatement :: E -> S -> FValueStatement -> Maybe (IO((S, FValueStatement)))
runValueStatement = undefined

putInState :: [FFunctionArg] -> [FValueStatement] -> E -> S -> S
putInState = undefined

registerArgs :: [FFunctionArg] -> E -> E
registerArgs = undefined

registerStates :: E -> String -> [FStructField] -> S
registerStates = undefined