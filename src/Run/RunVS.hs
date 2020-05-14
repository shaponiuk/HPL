module Run.RunVS where

import StaticCheck.Format
import Util.Util
import Util.State
import Util.Env

runVS :: Int -> FValueStatement -> E -> S -> IO (S, E, FValueStatement)
runVS queueId (FSusValueStatement vs) e s = do
    let queueId = getFreeQueueId s
    let ns = putQueue s (QueueT e vs queueId False False [] [])
    return (ns, e, FSusValueStatement queueId)
runVS queueId vs e s = traceD ("runVS " ++ show vs) undefined

runQueue :: QueueT -> S -> IO S
runQueue = undefined