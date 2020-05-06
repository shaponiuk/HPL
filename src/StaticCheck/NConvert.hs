module StaticCheck.NConvert where

import StaticCheck.Format
import Util.State
import Util.Env
import Data.Map as M
import Debug.Trace

convertToNPF :: ProgramFormat -> NProgramFormat
convertToNPF (SITList functions refs _) =
    trace "here" $
    NSIT env2 state2 where
        env = makeEnvForFunctions functions getNewEnv
        env2 = makeEnvForRefs refs env
        state1 = convertFunctions env2 getNewState functions
        state2 = convertRefs env2 state1 refs

makeEnvForFunctions :: [FFunctionDef] -> E -> E
makeEnvForFunctions = undefined

makeEnvForRefs :: [FRefDef] -> E -> E
makeEnvForRefs = undefined

convertFunctions :: E -> S -> [FFunctionDef] -> S
convertFunctions env = Prelude.foldl $ convertFunction env

convertFunction :: E -> S -> FFunctionDef -> S
convertFunction env s (NonSusFFunctionDef t name argNames vs) = undefined
    ns where
        locs = lookupLoc name env
        ns = registerInRightLocs locs argNames t vs s

registerInRightLocs :: [Int] -> [FPatternMatch] -> FType -> FValueStatement -> S -> S

convertRefs :: E -> S -> [FRefDef] -> S
convertRefs env = Prelude.foldl $ convertRef env

convertRef :: E -> S -> FRefDef -> S
convertRef = undefined