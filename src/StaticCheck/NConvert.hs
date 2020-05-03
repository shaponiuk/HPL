module StaticCheck.NConvert where

import StaticCheck.Format
import Util.State
import Util.Env
import Data.Map as M
import Debug.Trace

convertToNPF :: ProgramFormat -> NProgramFormat
convertToNPF (SITList structs algTypes) =
    NSIT nfStructs new_state where
    (nfStructs, new_state) = 
        Prelude.foldl (\(nfstrcts, st) strct ->
            let 
                (nfstrct, newState) = fstructConvert st strct
            in (nfstrct:nfstrcts, newState)
        ) ([], getNewState) structs
    
fstructConvert :: S -> FStruct -> (NFStruct, S)
fstructConvert s (FStructB name body) =
    (NFStruct name (NFStructBody publicNonSus), state) where
    (publicNonSus, state) = convertPublicNonSusFuns body s

convertPublicNonSusFuns :: [FStructField] -> S -> ([AnyDef], S)
convertPublicNonSusFuns structFields state =
    let
        (inStructEnv, newState) = makeInStructEnv getNewEnv structFields state
    in convertPublicNonSusFunFields structFields inStructEnv newState

makeInStructEnv :: E -> [FStructField] -> S -> (E, S)
makeInStructEnv env fields state =
    Prelude.foldl makeInStructEnvInFoldF (env, state) fields

makeInStructEnvInFoldF :: (E, S) -> FStructField -> (E, S)
makeInStructEnvInFoldF (e, s) (FStructFieldFunPublic (NonSusFFunctionDef _ name _ _)) =
    let
        (loc, newState) = getNewLoc s
        newEnv = registerLoc True e name loc
    in (newEnv, newState)
makeInStructEnvInFoldF (e, s) (FStructFieldFunPublic (SusFFunctionDef (NonSusFFunctionDef t name argNames vs))) =
    let
        (loc, newState) = getNewLoc s
        newEnv = registerLoc True e name loc
    in (newEnv, newState)
makeInStructEnvInFoldF (e, s) x = trace (show x) undefined

convertPublicNonSusFunFields :: [FStructField] -> E -> S -> ([AnyDef], S)
convertPublicNonSusFunFields fields env state = 
    Prelude.foldl (\(l, s) field ->
        let 
            (nf, ns) = convertPublicNonSusFunField field env s
        in (nf:l, ns)
    ) ([], state) fields

convertPublicNonSusFunField :: FStructField -> E -> S -> (AnyDef, S)
convertPublicNonSusFunField (FStructFieldFunPublic (NonSusFFunctionDef t name args vs)) env state =
    (NonSusFunDef $ NFNonSusFunDef name args env, newerState) where
        locs = lookupLoc name env
        loc = getUnsetLoc state locs 
        newState = putInLoc loc newThing state
        newerState = putArgNames newState loc args
        newThing = (True, env, t, vs)
convertPublicNonSusFunField (FStructFieldFunPublic (SusFFunctionDef (NonSusFFunctionDef t name args vs))) env state =
    (SusFunDef $ NFSusFunDef name args env, newerState) where
        locs = lookupLoc name env
        loc = getUnsetLoc state locs
        newState = putInLoc loc newThing state
        newerState = putArgNames newState loc args 
        newThing = (True, env, t, FSusValueStatement vs)