module Run.Run where

import StaticCheck.Format
import Util.Util
import Util.Env
import Util.State
import Run.RunVS

run :: NProgramFormat -> IO ()
run (NSIT env state) = do
    let mainloc = lookupFirstLoc "main" env
    let (nenv, vs) = stateLookup mainloc state
    (state, _) <- runVS 0 vs nenv state
    runLoopSingle state

runLoopSingle :: S -> IO ()
runLoopSingle state =
    if anyAvailibleQueue state then do
        let availibleQueue = getAvailibleQueue state
        _ <- runQueue availibleQueue state
        return ()
        -- runLoop newState
    else
        fail "No availible queue to run"

