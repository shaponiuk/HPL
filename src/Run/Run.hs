module Run.Run where

import StaticCheck.Format
import Util.Util
import Util.Env
import Util.State
import Run.OneStepEvaluation
import Debug.Trace

run :: NProgramFormat -> IO ()
run (NSIT env state) = do
    let (nloc, nstate) = getNewLoc state
    let mainloc = lookupFirstLoc "main" env
    let (_, nenv, t, vs) = stateLookup mainloc nstate
    let nnstate = putInLoc nloc (False, nenv, t, vs) nstate
    Just (s, vsf) <- interpretVS 0 vs nenv [] nloc nnstate []
    runLoop s

runLoop :: S -> IO ()
runLoop state =
    -- print $ queues state
    if anyAvailibleQueue state then do
        let availibleQueue = getAvailibleQueue state
        -- print availibleQueue
        newState <- runQueue availibleQueue state
        runLoop newState
    else
        putStrLn "No availible queue to run"

runQueue :: (E, FValueStatement, Int, Bool) -> S -> IO S
runQueue (env, vs, queueId, _) state = do
    print $ "running queue with id " ++ show queueId
    Just (ns, nvs) <- runVS queueId vs env state
    return $ putInQueue queueId (env, nvs, queueId, True) ns

interpretVS :: Int -> FValueStatement -> E -> [FPatternMatch] -> Int -> FunRunT
interpretVS queueId vs env argNames loc s vss =
    if length vss < length argNames
        then
            let (s_, vs) = wrapFunction vs argNames vss env s in
                runVS queueId vs env s_
        else do
            Just (ns, nvs) <- let (newEnv, newState) = registerArgs env s argNames vss in 
                runVS queueId vs newEnv newState
            let (b, t, e, _) = stateLookup loc ns
            let nns = putInLoc loc (b, t, e, nvs) ns
            return $ Just (nns, nvs)

runSpecialFunction :: Int -> FValueStatement -> E -> FunRunQuickT
runSpecialFunction queueId (FAValueStatement ap@(FFunApplicationB x r)) e s = do
    (nvs, ns, _, _) <- forceRunFunApplication queueId ap s e
    return $ Just (ns, nvs)
runSpecialFunction _ a _ _ = trace (show a) undefined

runVS :: Int -> FValueStatement -> E -> FunRunQuickT
runVS queueId (FForceValueStatement assignments vs) env state = do
    (newState, newEnv) <- forceRegisterAssignments queueId assignments state env
    seq newState $ runVS queueId vs newEnv newState
runVS queueId vs@(FIValueStatement i) _ s = return $ Just (s, vs)
runVS queueId a@(FAValueStatement (FFunApplicationB funName funArgVss)) env oldState =
    if funName == "print" || funName == "get" || funName == "set" || funName == "v" || funName == "p" || funName == "make_semaphore" || funName == "yield"
    then
        runSpecialFunction queueId a env oldState
    else do
        let firstLoc = lookupFirstLoc funName env
        let locs = lookupLoc funName env
        let funArgNames = funArgNamesLookup oldState firstLoc
        let (ifFunction, _, _, vs) = stateLookup firstLoc oldState
        (state, almostComputedVss) <- foldl (runVSInFoldF env) (return (oldState, [])) funArgVss
        let computedVss = reverse almostComputedVss
        print computedVss
        print funArgNames
        if length computedVss < length funArgNames
            then 
                let 
                    (nnnState, wrappedFunctionVS) = wrapFunctionB a env oldState
                in runVS queueId wrappedFunctionVS env nnnState
            else if ifFunction
            then
                if length computedVss > length funArgNames
                    then do
                        let (e, updatedVS, b, pms) = forceUnwrapMaybe $ appendFAVS locs computedVss state
                        if b
                            then
                                let (newEnv, newState) = registerArgs e state pms computedVss in 
                                    runVS queueId updatedVS newEnv newState
                            else
                                runVS queueId updatedVS e state
                    else 
                        tryRunVSFunApplR queueId locs computedVss state
            else if length computedVss > length funArgNames
                then do
                    let (e, updatedVS, b, pms) = forceUnwrapMaybe $ appendFAVS locs computedVss state
                    if b
                        then
                            let (newEnv, newState) = registerArgs e state pms computedVss in 
                                runVS queueId updatedVS newEnv newState
                        else
                            runVS queueId updatedVS e state
                else 
                    interpretVS queueId vs env funArgNames firstLoc oldState funArgVss
runVS queueId (FExpr (FEMul vs1 vs2)) env state = do
    Just (newState, FIValueStatement i1) <- runVS queueId vs1 env state
    Just (newerState, FIValueStatement i2) <- runVS queueId vs2 env newState
    return $ Just (newerState, FIValueStatement (i1 * i2))
runVS queueId (FIfValueStatement condvs res1vs res2vs) env state = do
    Just (newState, FIValueStatement condVal) <- runVS queueId condvs env state
    if condVal /= 0
        then runVS queueId res1vs env newState
        else runVS queueId res2vs env newState
runVS queueId (FExpr (FEEQ vs1 vs2)) env state = do
    Just (newState, FIValueStatement i1) <- runVS queueId vs1 env state
    Just (newerState, FIValueStatement i2) <- runVS queueId vs2 env newState
    return $ Just (newerState, FIValueStatement $ if i1 == i2 then 1 else 0)
runVS queueId (FExpr (FESub vs1 vs2)) env state = do
    Just (newState, FIValueStatement i1) <- runVS queueId vs1 env state
    Just (newerState, FIValueStatement i2) <- runVS queueId vs2 env newState
    return $ Just (newerState, FIValueStatement (i1 - i2))
runVS queueId (FExpr (FEAdd vs1 vs2)) env state = do
    Just (newState, FIValueStatement i1) <- runVS queueId vs1 env state
    Just (newerState, FIValueStatement i2) <- runVS queueId vs2 env newState
    return $ Just (newerState, FIValueStatement (i1 + i2))
runVS queueId (FAValueStatement (FFunApplicationR locs args)) _ state =
    tryRunVSFunApplR queueId locs args state
runVS queueId vs@(FLitStrValueStatement _) _ s = return $ Just (s, vs)
runVS queueId (FCValueStatement name vss) e s = do
    (new_state, newVss) <- foldl (cvsInFoldF queueId e) (return (s, [])) vss
    return $ Just (new_state, FCValueStatement name (reverse newVss))
runVS queueId (FTValueStatement xss) e s = do
    (ns, xssc) <- foldl (runTVSInFoldF queueId e) (return (s, [])) xss
    return $ Just (ns, FTValueStatement xssc)
runVS queueId a@(FRefAddr x) _ s = return $ Just (s, a)
runVS queueId a@(FFValueStatement argName vs) e s =
    return $ Just (s, a)
runVS queueId (FSusValueStatement vs) e s = do
    let queueId = getFreeQueueId s
    let nvs = FSuspendedValue queueId
    let ns = putQueue s (e, vs, queueId, False)
    return $ Just (ns, nvs)
runVS queueId (FSuspendedValue qId) e s = do
    -- print $ "runVS suspended value " ++ show qId
    let (env, qvs, _, b) = getQueue qId s
    Just (ns, nvs) <- runVS qId qvs env s
    let nns = putInQueue qId (env, qvs, qId, True) ns
    return $ Just (nns, nvs)
runVS queueId vs@(FSemaphore i) e s =
    return $ Just (s, vs)

runVS _ vs _ _ = trace ("runVS??? " ++ show vs) undefined

runTVSInFoldF :: Int -> E -> IO (S, [FValueStatement]) -> FValueStatement -> IO (S, [FValueStatement])
runTVSInFoldF queueId env acc vs = do
    (s, vsl) <- acc
    Just (ns, nvs) <- runVS queueId vs env s
    return (ns, nvs:vsl)

cvsInFoldF :: Int -> E -> IO (S, [FValueStatement]) -> FValueStatement -> IO (S, [FValueStatement])
cvsInFoldF queueId e acc vs = do
    (state, vsl) <- acc
    Just (new_state, vs_) <- runVS queueId vs e state
    return (new_state, vs_:vsl)

tryRunVSFunApplR :: Int -> [Int] -> [FValueStatement] -> FunRunQuickT
    -- todo: here might fail
tryRunVSFunApplR _ [] _ _ = undefined
-- todo: hard to find the function name, probably unneeded, only one at the time
tryRunVSFunApplR queueId (x:xs) args state = do
    let (ifFunction, env, _, vs) = stateLookup x state
    let argNames = funArgNamesLookup state x
    if fitPatternMatchs state env argNames args
        then
            if length args > length argNames
                then do
                    let (e, updatedVS, b, pms) = forceUnwrapMaybe $ appendFAVS [x] args state
                    if b
                        then
                            let (newEnv, newState) = registerArgs e state pms args in 
                                runVS queueId updatedVS newEnv newState
                        else 
                            runVS queueId updatedVS e state
                else
                    let (newEnv, newState) = registerArgs env state argNames args in 
                        runVS queueId vs newEnv newState
        else 
            tryRunVSFunApplR queueId xs args state

runVSInFoldF :: E -> IO (S, [FValueStatement]) -> FValueStatement -> IO (S, [FValueStatement])
runVSInFoldF env acc vs = do
    (s, vsl) <- acc
    let nvs = convertBApplicationsToRApplications vs env
    return (s, nvs:vsl)

convertBApplicationsToRApplications :: FValueStatement -> E -> FValueStatement
convertBApplicationsToRApplications (FExpr (FESub vs1 vs2)) e =
    FExpr $ FESub nvs1 nvs2 where
        nvs1 = convertBApplicationsToRApplications vs1 e
        nvs2 = convertBApplicationsToRApplications vs2 e
convertBApplicationsToRApplications (FAValueStatement (FFunApplicationB funName funArgs)) e =
    FAValueStatement $ FFunApplicationR locs funArgs where
        locs = lookupLoc funName e
convertBApplicationsToRApplications a@(FIValueStatement _) _ = a
convertBApplicationsToRApplications (FCValueStatement name vss) env =
    FCValueStatement name $ map (`convertBApplicationsToRApplications` env) vss
convertBApplicationsToRApplications (FTValueStatement vss) env =
    FTValueStatement $ map (`convertBApplicationsToRApplications` env) vss
convertBApplicationsToRApplications a b = trace ("convertBApplicationsToRApplications " ++ show a) undefined

wrapFunction :: FValueStatement -> [FPatternMatch] -> [FValueStatement] -> E -> S -> (S, FValueStatement)
wrapFunction vs pms vss env s = undefined

wrapFunctionB :: FValueStatement -> E -> S -> (S, FValueStatement)
wrapFunctionB vs@(FAValueStatement (FFunApplicationB x vss)) env state =
    let
        locs = lookupLoc x env
    in wrapFunctionBInt locs vs env state

wrapFunctionBInt :: [Int] -> FValueStatement -> E -> S -> (S, FValueStatement)
wrapFunctionBInt (loc:locs) vs@(FAValueStatement (FFunApplicationB x vss)) env state =
    let
        funArgNames = funArgNamesLookup state loc
    in if fitPatternMatchs state env funArgNames vss
        then
            let 
                d = length funArgNames - length vss
                (newLocs, newState) = getNNewLocs state d
                lambdaNames = map show newLocs
            in (newState, wrapFunctionBIntNNewLambdas d vs lambdaNames)
        else 
            wrapFunctionBInt locs vs env state

wrapFunctionBIntNNewLambdas :: Int -> FValueStatement -> [String] -> FValueStatement
wrapFunctionBIntNNewLambdas d (FAValueStatement (FFunApplicationB x vss)) strs = 
    let
        argVss = map makeFunApplicationNoArg strs
    in foldl (flip FFValueStatement) (FAValueStatement (FFunApplicationB x (vss ++ argVss))) strs

makeFunApplicationNoArg :: String -> FValueStatement
makeFunApplicationNoArg x = FAValueStatement (FFunApplicationB x [])

forceRegisterAssignments :: Int -> [FAssignment] -> S -> E -> IO (S, E)
forceRegisterAssignments queueId assignments state env =
    foldl (forceRegisterAssignmentsInFoldF queueId) (return (state, env)) assignments

forceRegisterAssignmentsInFoldF :: Int -> IO (S, E) -> FAssignment -> IO (S, E)
forceRegisterAssignmentsInFoldF queueId acc assignment = do
    (s, e) <- acc
    forceRegisterAssignment queueId assignment s e

forceRegisterAssignment :: Int -> FAssignment -> S -> E -> IO (S, E)
forceRegisterAssignment queueId a@(FAssignmentB typ@(FTypeB "Ref" [t]) (FPatternMatchB name) vs) state env = do
    let (refLoc, newState) = getNewLoc state
    let (vsLoc, newerState) = getNewLoc newState
    let refVS = FRefAddr vsLoc
    let newEnv = registerLoc False env name refLoc
    let newererState = putInLoc refLoc (False, newEnv, typ, refVS) newerState
    Just (newerererState, nvs) <- runVS queueId vs newEnv newererState
    let newererererState = putInLoc vsLoc (False, newEnv, t, nvs) newerererState
    return (newererererState, newEnv)
forceRegisterAssignment queueId a@(FAssignmentB t pm vs) state env =
    setPM queueId t pm vs state env
forceRegisterAssignment queueId a _ _ = trace (show a) undefined

setPM :: Int -> FType -> FPatternMatch -> FValueStatement -> S -> E -> IO (S, E)
setPM qId (FTypeT types) (FPatternMatchT pmL) vs state env = do
    (vss, newState, newEnv, _) <- forceGetTupleVSS qId vs state env
    foldl (setPMInFoldF qId) (return (newState, newEnv)) $ tList types pmL vss
setPM qId t (FPatternMatchB x) vs state env = do
    let (loc, newState) = getNewLoc state
    let newEnv = registerLoc False env x loc
    Just (newerState, nvs) <- interpretVS qId vs newEnv [] loc newState []
    let newererState = putInLoc loc (False, newEnv, t, nvs) newerState
    return (newererState, newEnv)
setPM _ _ _ _ _ _ = undefined

setPMInFoldF :: Int -> IO (S, E) -> (FType, FPatternMatch, FValueStatement) -> IO (S, E)
setPMInFoldF queueId acc (t, pm, vs) = do
    (s, e) <- acc
    setPM queueId t pm vs s e

unwrapSingleTuples :: FValueStatement -> [FValueStatement]
unwrapSingleTuples (FTValueStatement [vs]) = unwrapSingleTuples vs
unwrapSingleTuples (FTValueStatement vss) = vss
unwrapSingleTuples a = [a]

forceGetTupleVSS :: Int -> FValueStatement -> S -> E -> IO ([FValueStatement], S, E, Bool)
forceGetTupleVSS queueId a@(FTValueStatement vss) s e = do
    Just (ns, tvs) <- runVS queueId a e s
    return (unwrapSingleTuples tvs, ns, e, False)
forceGetTupleVSS queueId a@(FAValueStatement funApplication) state env = do
    -- print "forceGetTupleVSS"
    -- print a
    -- print env
    (vs, s, e, b) <- forceRunFunApplication queueId funApplication state env
    -- print vs
    forceGetTupleVSS queueId vs s e
forceGetTupleVSS qId vs@(FSuspendedValue queueId) s e =
    return ([vs], s, e, False)
forceGetTupleVSS _ vs _ _ = trace (show vs) undefined

-- bool -> if should stop
forceRunFunApplication :: Int -> FFunApplication -> S -> E -> IO (FValueStatement, S, E, Bool)
forceRunFunApplication queueId (FFunApplicationB "print" [str]) state env = do
    Just (s, r) <- runVS queueId str env state
    case r of
        FSuspendedValue qId -> do
            Just (ns, nr) <- runVS qId r env s
            print $ "hehe " ++ show nr 
            return (FTValueStatement [], ns, env, False)
        _ -> do
            print $ "hehe " ++ show r
            return (FTValueStatement [], s, env, False)
forceRunFunApplication queueId (FFunApplicationB "set" [ref, value]) state env = do
    Just (newState, FRefAddr refAddr) <- runVS queueId ref env state
    let newerState = putInLoc refAddr (False, env, FTypeT [], value) newState
    return (FTValueStatement [], newerState, env, False)
forceRunFunApplication queueId (FFunApplicationB "get" [ref]) state env = do
    -- Just (newState, aaa) <- runVS queueId ref env state
    -- print ref
    -- print aaa
    -- print env
    -- print newState
    -- undefined
    Just (newState, FRefAddr refAddr) <- runVS queueId ref env state
    let (_, e, _, refVS) = stateLookup refAddr newState
    Just (newerState, vs) <- runVS queueId refVS e newState
    return (vs, newerState, e, False)
forceRunFunApplication queueId (FFunApplicationB "p" [semref]) state env = do
    Just (newState, FSemaphore semId) <- runVS queueId semref env state
    let (blockedQueues, semValue, _) = getSemaphore semId state
    if semValue <= 0
        then do
            let newerState = putSemaphore semId (queueId:blockedQueues, semValue, semId) newState
            let availibleQueue = getAvailibleQueue newerState
            runUntilSemReady queueId semId newerState env
        else do
            let newerState = putSemaphore semId (blockedQueues, semValue - 1, semId) newState
            return (FTValueStatement [], newerState, env, False)
forceRunFunApplication queueId (FFunApplicationB "make_semaphore" []) state env = do
    let (([], 0, semId), newState) = getNewSemaphore state
    return (FSemaphore semId, newState, env, False)
forceRunFunApplication queueId (FFunApplicationB "v" [semref]) state env = do
    Just (newState, FSemaphore semId) <- runVS queueId semref env state
    let (blockedQueues, semValue, _) = getSemaphore semId newState
    if null blockedQueues
        then do
            let newerState = putSemaphore semId ([], semValue + 1, semId) newState
            return (FTValueStatement [], newerState, env, False)
        else do
            let newerState = putSemaphore semId (cutLast blockedQueues, semValue, semId) newState
            return (FTValueStatement [], newerState, env, False)
forceRunFunApplication queueId a@(FFunApplicationB name args) state env = do
    Just (s, vs) <- runVS queueId (FAValueStatement a) env state
    -- print $ "after " ++ show a
    -- print vs
    return (vs, s, env, False)
forceRunFunApplication queueId a@(FFunApplicationR loc [args]) state env = do
    Just (s, vs) <- runVS queueId (FAValueStatement a) env state
    return (vs, s, env, False)
forceRunFunApplication queueId a _ _ = trace ("forceRunFunApplication " ++ show a) undefined

runUntilSemReady :: Int -> Int -> S -> E -> IO (FValueStatement, S, E, Bool)
runUntilSemReady queueId semId state env = do
    print $ "runUnitlSemReady " ++ show queueId
    if anyAvailibleQueue state
        then do
            let availibleQueue = getAvailibleQueue state
            newState <- runQueue availibleQueue state
            let queue = getQueue queueId newState
            if checkNotBlocked queue newState
                then return (FTValueStatement [], newState, env, False)
                else runUntilSemReady queueId semId state env
        else fail "Deadlock"