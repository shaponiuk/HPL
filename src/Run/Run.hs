module Run.Run where

import StaticCheck.Format
import Util.Util
import Util.Env
import Util.State
import Run.RunVS

run :: NProgramFormat -> IO ()
run (NSIT env state) = do
    let mainloc = lookupFirstLoc "main" env
    let (_, nenv, t, vs) = stateLookup mainloc state
    (state, _, _) <- runVS 0 vs nenv state
    runLoop state

runLoop :: S -> IO ()
runLoop state =
    if anyAvailibleQueue state then do
        let availibleQueue = getAvailibleQueue state
        newState <- runQueue availibleQueue state
        runLoop newState
    else
        printD "No availible queue to run"

