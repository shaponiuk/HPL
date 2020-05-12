module StaticCheck.NConvert where

import StaticCheck.Format
import Util.State
import Util.Env
import Data.Map as M

convertToNPF :: ProgramFormat -> NProgramFormat
convertToNPF (SITList functions refs _) =
    NSIT env2 state2 where
        (env, state0_) = makeEnvForFunctions functions getNewEnv getNewState
        (env2, state0) = makeEnvForRefs refs env state0_
        state1 = convertFunctions env2 state0 functions
        state2 = convertRefs env2 state1 refs

makeEnvForFunctions :: [FFunctionDef] -> E -> S -> (E, S)
makeEnvForFunctions l env s = Prelude.foldl makeEnvForFunction (env, s) l

makeEnvForFunction :: (E, S) -> FFunctionDef -> (E, S)
makeEnvForFunction (e, s) (NonSusFFunctionDef _ name _ _) =
    (ne, ns) where
        (loc, ns) = getNewLoc s
        ne = registerLoc True e name loc
makeEnvForFunction (e, s) (SusFFunctionDef (NonSusFFunctionDef _ name _ _)) =
    (ne, ns) where
        (loc, ns) = getNewLoc s
        ne = registerLoc True e name loc

makeEnvForRefs :: [FRefDef] -> E -> S -> (E, S)
makeEnvForRefs l env s = Prelude.foldl makeEnvForRef (env, s) l

makeEnvForRef :: (E, S) -> FRefDef -> (E, S)
makeEnvForRef (e, s) (FRefDef _ name _) =
    (ne, ns) where
        (loc, ns) = getNewLoc s
        ne = registerLoc False e name loc

convertFunctions :: E -> S -> [FFunctionDef] -> S
convertFunctions env = Prelude.foldl $ convertFunction env

convertFunction :: E -> S -> FFunctionDef -> S
convertFunction env s (NonSusFFunctionDef t name argNames vs) =
    ns where
        locs = lookupLoc name env
        loc = getUnsetLoc s locs
        ns_ = putArgNames s loc argNames
        ns = putInLoc loc (True, env, t, vs) ns_
convertFunction env s (SusFFunctionDef (NonSusFFunctionDef t name argNames vs)) =
    ns where
        locs = lookupLoc name env
        loc = getUnsetLoc s locs
        ns_ = putArgNames s loc argNames
        ns = putInLoc loc (True, env, t, FSusValueStatement vs) ns_

convertRefs :: E -> S -> [FRefDef] -> S
convertRefs env = Prelude.foldl $ convertRef env

unrefType :: FType -> FType
unrefType (FTypeB "Ref" [t]) = t

convertRef :: E -> S -> FRefDef -> S
convertRef env state (FRefDef t name vs) =
    ns where
        loc = lookupFirstLoc name env
        (newLoc, ns_) = getNewLoc state
        ns__ = putInLoc loc (False, env, t, FRefAddr newLoc) ns_
        ns = putInLoc newLoc (False, env, unrefType t, vs) ns__