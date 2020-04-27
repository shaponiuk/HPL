module StaticCheck.NConvert where

import StaticCheck.Format
import Debug.Trace

convertToNPF :: ProgramFormat -> NProgramFormat
convertToNPF (SITList structs interfaces algTypes) =
    trace (show $ length nfStructs) $ NSIT nfStructs interfaces algTypes state where
    (nfStructs, state) = 
        foldl (\(nfstrcts, st) strct ->
            let 
                (nfstrct, newState) = fstructConvert st strct
            in (nfstrct:nfstrcts, newState)
        ) ([], getNewState) structs

fstructConvert :: S -> FStruct -> (NFStruct, S)
fstructConvert s (FStructB name (FStructBody body)) =
    (NFStruct name [] (
        NFStructBody
        []
        publicNonSus
        []
        []
        []
        []),
    state) where
    (publicNonSus, state) = convertPublicNonSusFuns body s

getNewState :: S
getNewState = undefined

getNewEnv :: E
getNewEnv = undefined

convertPublicNonSusFuns :: [FStructField] -> S -> ([NFNonSusFunDef], S)
convertPublicNonSusFuns structFields state =
    let
        publicFields = extractPublicNonSusFunFields structFields
    in let
        (inStructEnv, newState) = makeInStructEnv getNewEnv structFields state
    in (convertPublicNonSusFunFields structFields inStructEnv newState)

makeInStructEnv :: E -> [FStructField] -> S -> (E, S)
makeInStructEnv = undefined

extractPublicNonSusFunFields :: [FStructField] -> [FStructField]
extractPublicNonSusFunFields = filter checkPublicNonSusFunctionField

checkPublicNonSusFunctionField :: FStructField -> Bool
checkPublicNonSusFunctionField (FStructFieldFunPublic (NonSusFFunctionDef _ _ _ _)) = True
checkPublicNonSusFunctionField _ = False

convertPublicNonSusFunFields :: [FStructField] -> E -> S -> ([NFNonSusFunDef], S)
convertPublicNonSusFunFields fields env state = foldl (\(l, s) field ->
    let 
        (nf, ns) = (convertPublicNonSusFunField field env state)
    in (nf:l, ns)

convertPublicNonSusFunField :: FStructField -> E -> S -> (NFNonSusFunDef, S)
convertPublicNonSusFunField (FStructFieldFunPublic (NonSusFFunctionDef t name args vs)) env state =
    (NFNonSusFunDef name args, newState) where
        