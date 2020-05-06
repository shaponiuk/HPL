module StaticCheck.NConvert where

import StaticCheck.Format
import Util.State
import Util.Env
import Data.Map as M
import Debug.Trace

convertToNPF :: ProgramFormat -> NProgramFormat
convertToNPF (SITList functions refs _) =
    trace "here" $
    NSIT (functionDefs ++ refDefs) state2 where
        env = makeEnvForFunctions functions getNewEnv
        env2 = makeEnvForRefs refs env
        (functionDefs, state1) = convertFunctions env2 getNewState functions
        (refDefs, state2) = convertRefs env2 state1 refs

makeEnvForFunctions :: [FFunctionDef] -> E -> E
makeEnvForFunctions = undefined

makeEnvForRefs :: [FRefDef] -> E -> E
makeEnvForRefs = undefined

convertFunctions :: E -> S -> [FFunctionDef] -> ([AnyDef], S)
convertFunctions env state = Prelude.foldl (convertFunction env) ([], state)

convertFunction :: E -> ([AnyDef], S) -> FFunctionDef -> ([AnyDef], S)
convertFunction = undefined

convertRefs :: E -> S -> [FRefDef] -> ([AnyDef], S)
convertRefs env state = Prelude.foldl (convertRef env) ([], state)

convertRef :: E -> ([AnyDef], S) -> FRefDef -> ([AnyDef], S)
convertRef = undefined