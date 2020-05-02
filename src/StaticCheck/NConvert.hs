module StaticCheck.NConvert where

import StaticCheck.Format
import Util.State
import Util.Env
import Data.Map as M
import Debug.Trace

convertToNPF :: ProgramFormat -> NProgramFormat
convertToNPF (SITList structs interfaces algTypes) =
    NSIT nfStructs new_state where
    (nfStructs, new_state) = 
        Prelude.foldl (\(nfstrcts, st) strct ->
            let 
                (nfstrct, newState) = fstructConvert st strct
            in (nfstrct:nfstrcts, newState)
        ) ([], getNewState) structs
    
fstructConvert :: S -> FStruct -> (NFStruct, S)
fstructConvert s (FStructB name body) =
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
getNewState = S M.empty 0 M.empty

getNewEnv :: E
getNewEnv = E M.empty

convertPublicNonSusFuns :: [FStructField] -> S -> ([NFNonSusFunDef], S)
convertPublicNonSusFuns structFields state =
    let
        publicFields = extractPublicNonSusFunFields structFields
    in let
        (inStructEnv, newState) = makeInStructEnv getNewEnv publicFields state
    in convertPublicNonSusFunFields publicFields inStructEnv newState

makeInStructEnv :: E -> [FStructField] -> S -> (E, S)
makeInStructEnv env fields state =
    Prelude.foldl (\(e, s) (FStructFieldFunPublic (NonSusFFunctionDef _ name _ _)) ->
        let
            (loc, newState) = getNewLoc s
            newEnv = registerLoc True e name loc
        in (newEnv, newState)
    ) (env, state) fields

extractPublicNonSusFunFields :: [FStructField] -> [FStructField]
extractPublicNonSusFunFields = Prelude.filter checkPublicNonSusFunctionField

checkPublicNonSusFunctionField :: FStructField -> Bool
checkPublicNonSusFunctionField (FStructFieldFunPublic NonSusFFunctionDef {}) = True
checkPublicNonSusFunctionField _ = False

convertPublicNonSusFunFields :: [FStructField] -> E -> S -> ([NFNonSusFunDef], S)
convertPublicNonSusFunFields fields env state = 
    Prelude.foldl (\(l, s) field ->
        let 
            (nf, ns) = convertPublicNonSusFunField field env s
        in (nf:l, ns)
    ) ([], state) fields

convertPublicNonSusFunField :: FStructField -> E -> S -> (NFNonSusFunDef, S)
convertPublicNonSusFunField (FStructFieldFunPublic (NonSusFFunctionDef t name args vs)) env state =
    (NFNonSusFunDef name args env, newerState) where
        locs = lookupLoc name env
        loc = getUnsetLoc state locs 
        newState = putInLoc loc newThing state
        newerState = putArgNames newState loc args
        newThing = (True, env, t, vs)
convertPublicNonSusFunField _ _ _ = undefined