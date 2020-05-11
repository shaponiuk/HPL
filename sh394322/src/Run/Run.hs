module Run.Run where

import StaticCheck.Format
import Util.Util
import Util.Env
import Util.State
import Run.OneStepEvaluation
import Run.RunVS

run :: NProgramFormat -> IO ()
run (NSIT env state) = do
    let (nloc, nstate) = getNewLoc state
    let mainloc = lookupFirstLoc "main" env
    let (_, nenv, t, vs) = stateLookup mainloc nstate
    let nnstate = putInLoc nloc (False, nenv, t, vs) nstate
    (s, vsf) <- interpretVS 0 vs nenv [] nloc nnstate []
    runLoop s

runLoop :: S -> IO ()
runLoop state =
    if anyAvailibleQueue state then do
        let availibleQueue = getAvailibleQueue state
        newState <- runQueue availibleQueue state
        runLoop newState
    else
        printD "No availible queue to run"

