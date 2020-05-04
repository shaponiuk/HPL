module StaticCheck.NConvert where

import StaticCheck.Format
import Util.State
import Util.Env
import Data.Map as M
import Debug.Trace

convertToNPF :: ProgramFormat -> NProgramFormat
convertToNPF (SITList functions refs algTypes) =
    trace "here" $
    NSIT (functionDefs ++ refDefs ++ algTypeDefs) state3 where
        env = makeEnvForFunctions functions getNewEnv
        env2 = makeEnvForRefs refs env
        (functionDefs, state1) = convertFunctions env2 getNewState functions
        (refDefs, state2) = convertRefs env2 state1 refs
        (algTypeDefs, state3) = convertAlgTypes env2 state3 algTypes

makeEnvForFunctions :: [FFunctionDef] -> E -> E
makeEnvForFunctions = undefined

makeEnvForRefs :: [FRefDef] -> E -> E
makeEnvForRefs = undefined

convertFunctions :: E -> S -> [FFunctionDef] -> ([AnyDef], S)
convertFunctions = undefined

convertRefs :: E -> S -> [FRefDef] -> ([AnyDef], S)
convertRefs = undefined

convertAlgTypes :: E -> S -> [FAlgType] -> ([AnyDef], S)
convertAlgTypes env state = Prelude.foldl (convertAlgType env) ([], state)

convertAlgType :: E -> ([AnyDef], S) -> FAlgType -> ([AnyDef], S)
convertAlgType env (l, s) (FAlgType name typeArgs constructors) = undefined