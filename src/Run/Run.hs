module Run.Run where

import StaticCheck.Format
import Util.Util
import Util.Env
import Util.State
import Run.OneStepEvaluation

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

runQueue :: QueueT -> S -> IO S
runQueue (QueueT env vs queueId _ _) state = do
    printD $ "running queue with id " ++ show queueId
    (ns, nvs) <- runVS queueId vs env state
    return $ putInQueue queueId (QueueT env nvs queueId True False) ns

interpretVS :: Int -> FValueStatement -> E -> [FPatternMatch] -> Int -> S -> [FValueStatement] -> IO (S, FValueStatement)
interpretVS queueId vs env argNames loc s vss =
    if length vss < length argNames
        then
            let (s_, vs) = wrapFunction vs argNames vss env s in
                runVS queueId vs env s_
        else do
            let (newEnv, newState) = registerArgs env s argNames vss
            (ns, nvs) <- runVS queueId vs newEnv newState
            let (b, t, e, _) = stateLookup loc ns
            let nns = putInLoc loc (b, t, e, nvs) ns
            return (nns, nvs)

runSpecialFunction :: Int -> FValueStatement -> E -> S -> IO (S, FValueStatement)
runSpecialFunction queueId (FAValueStatement ap@(FFunApplicationB x r)) e s = do
    (nvs, ns, _) <- forceRunFunApplication queueId ap s e
    return (ns, nvs)
runSpecialFunction _ a _ _ = traceD (show a) undefined

checkSpecialFunctionName :: String -> Bool
checkSpecialFunctionName funName =
    funName `elem` ["print", "get", "set", "v", "p", "make_semaphore", "yield"]

runNotSpecialFunction :: Int -> FValueStatement -> E -> S -> IO (S, FValueStatement)
runNotSpecialFunction queueId a@(FAValueStatement (FFunApplicationB funName funArgVss)) env oldState = do
    let firstLoc = lookupFirstLoc funName env
    let locs = lookupLoc funName env
    let funArgNames = funArgNamesLookup oldState firstLoc
    let (ifFunction, _, _, vs) = stateLookup firstLoc oldState
    (state, almostComputedVss) <- foldl (runVSInFoldF env) (return (oldState, [])) funArgVss
    let computedVss = reverse almostComputedVss
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

runVS :: Int -> FValueStatement -> E -> S -> IO (S, FValueStatement)
runVS queueId (FForceValueStatement assignments vs) env state = do
    (newState, newEnv) <- forceRegisterAssignments queueId assignments state env
    seq newState $ runVS queueId vs newEnv newState
runVS queueId vs@(FIValueStatement i) _ s = return (s, vs)
runVS queueId a@(FAValueStatement (FFunApplicationB funName funArgVss)) env state =
    if checkSpecialFunctionName funName
        then
            runSpecialFunction queueId a env state
        else 
            runNotSpecialFunction queueId a env state
runVS queueId (FExpr (FEMul vs1 vs2)) env state = do
    (newState, FIValueStatement i1) <- runVS queueId vs1 env state
    (newerState, FIValueStatement i2) <- runVS queueId vs2 env newState
    return (newerState, FIValueStatement (i1 * i2))
runVS queueId (FIfValueStatement condvs res1vs res2vs) env state = do
    (newState, FIValueStatement condVal) <- runVS queueId condvs env state
    if condVal /= 0
        then runVS queueId res1vs env newState
        else runVS queueId res2vs env newState
runVS queueId (FExpr (FEEQ vs1 vs2)) env state = do
    (newState, FIValueStatement i1) <- runVS queueId vs1 env state
    (newerState, FIValueStatement i2) <- runVS queueId vs2 env newState
    return (newerState, FIValueStatement $ if i1 == i2 then 1 else 0)
runVS queueId (FExpr (FESub vs1 vs2)) env state = do
    (newState, FIValueStatement i1) <- runVS queueId vs1 env state
    (newerState, FIValueStatement i2) <- runVS queueId vs2 env newState
    return (newerState, FIValueStatement (i1 - i2))
runVS queueId (FExpr (FEAdd vs1 vs2)) env state = do
    (newState, FIValueStatement i1) <- runVS queueId vs1 env state
    (newerState, FIValueStatement i2) <- runVS queueId vs2 env newState
    return (newerState, FIValueStatement (i1 + i2))
runVS queueId (FAValueStatement (FFunApplicationR locs args)) _ state =
    tryRunVSFunApplR queueId locs args state
runVS queueId vs@(FLitStrValueStatement _) _ s = return (s, vs)
runVS queueId (FCValueStatement name vss) e s = do
    (new_state, newVss) <- foldl (cvsInFoldF queueId e) (return (s, [])) vss
    return (new_state, FCValueStatement name (reverse newVss))
runVS queueId (FTValueStatement xss) e s = do
    (ns, xssc) <- foldl (runTVSInFoldF queueId e) (return (s, [])) xss
    return (ns, FTValueStatement xssc)
runVS queueId a@(FRefAddr x) _ s = return (s, a)
runVS queueId a@(FFValueStatement argName vs) e s =
    return (s, a)
runVS queueId (FSusValueStatement vs) e s = do
    let queueId = getFreeQueueId s
    let nvs = FSuspendedValue queueId
    let ns = putQueue s (QueueT e vs queueId False False)
    return (ns, nvs)
runVS queueId (FSuspendedValue qId) e s = do
    let (QueueT env qvs _ b y) = getQueue qId s
    (ns, nvs) <- runVS qId qvs env s
    let nns = putInQueue qId (QueueT env qvs qId True False) ns
    return (nns, nvs)
runVS queueId vs@(FSemaphore i) e s =
    return (s, vs)

runVS _ vs _ _ = traceD ("runVS??? " ++ show vs) undefined

runTVSInFoldF :: Int -> E -> IO (S, [FValueStatement]) -> FValueStatement -> IO (S, [FValueStatement])
runTVSInFoldF queueId env acc vs = do
    (s, vsl) <- acc
    (ns, nvs) <- runVS queueId vs env s
    return (ns, nvs:vsl)

cvsInFoldF :: Int -> E -> IO (S, [FValueStatement]) -> FValueStatement -> IO (S, [FValueStatement])
cvsInFoldF queueId e acc vs = do
    (state, vsl) <- acc
    (new_state, vs_) <- runVS queueId vs e state
    return (new_state, vs_:vsl)

tryRunVSFunApplR :: Int -> [Int] -> [FValueStatement] -> S -> IO (S, FValueStatement)
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
convertBApplicationsToRApplications a b = traceD ("convertBApplicationsToRApplications " ++ show a) undefined

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
    (newerererState, nvs) <- runVS queueId vs newEnv newererState
    let newererererState = putInLoc vsLoc (False, newEnv, t, nvs) newerererState
    return (newererererState, newEnv)
forceRegisterAssignment queueId a@(FAssignmentB t pm vs) state env =
    setPM queueId t pm vs state env
forceRegisterAssignment queueId a _ _ = traceD (show a) undefined

setPM :: Int -> FType -> FPatternMatch -> FValueStatement -> S -> E -> IO (S, E)
setPM qId (FTypeT types) (FPatternMatchT pmL) vs state env = do
    (vss, newState, newEnv, _) <- forceGetTupleVSS qId vs state env
    foldl (setPMInFoldF qId) (return (newState, newEnv)) $ tList types pmL vss
setPM qId t (FPatternMatchB x) vs state env = do
    let (loc, newState) = getNewLoc state
    let newEnv = registerLoc False env x loc
    (newerState, nvs) <- interpretVS qId vs newEnv [] loc newState []
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
    (ns, tvs) <- runVS queueId a e s
    return (unwrapSingleTuples tvs, ns, e, False)
forceGetTupleVSS queueId a@(FAValueStatement funApplication) state env = do
    (vs, s, e) <- forceRunFunApplication queueId funApplication state env
    forceGetTupleVSS queueId vs s e
forceGetTupleVSS qId vs@(FSuspendedValue queueId) s e =
    return ([vs], s, e, False)
forceGetTupleVSS _ vs _ _ = traceD (show vs) undefined

forceRunFunApplication :: Int -> FFunApplication -> S -> E -> IO (FValueStatement, S, E)
forceRunFunApplication queueId (FFunApplicationB "print" [str]) state env = do
    (s, r) <- runVS queueId str env state
    case r of
        FSuspendedValue qId -> do
            (ns, nr) <- runVS qId r env s
            print $ "hehe " ++ show nr 
            return (FTValueStatement [], ns, env)
        _ -> do
            print $ "hehe " ++ show r
            return (FTValueStatement [], s, env)
forceRunFunApplication queueId (FFunApplicationB "set" [ref, value]) state env = do
    (newState, FRefAddr refAddr) <- runVS queueId ref env state
    let newerState = putInLoc refAddr (False, env, FTypeT [], value) newState
    return (FTValueStatement [], newerState, env)
forceRunFunApplication queueId (FFunApplicationB "get" [ref]) state env = do
    (newState, FRefAddr refAddr) <- runVS queueId ref env state
    let (_, e, _, refVS) = stateLookup refAddr newState
    (newerState, vs) <- runVS queueId refVS e newState
    return (vs, newerState, e)
forceRunFunApplication queueId (FFunApplicationB "p" [semref]) state env = do
    (newState, FSemaphore semId) <- runVS queueId semref env state
    let (SemaphoreT blockedQueues semValue _) = getSemaphore semId state
    if semValue <= 0
        then do
            let newerState = putSemaphore semId (SemaphoreT (queueId:blockedQueues) semValue semId) newState
            let availibleQueue = getAvailibleQueue newerState
            runUntilSemReady queueId semId newerState env
        else do
            let newerState = putSemaphore semId (SemaphoreT blockedQueues (semValue - 1) semId) newState
            return (FTValueStatement [], newerState, env)
forceRunFunApplication queueId (FFunApplicationB "yield" []) state env =
    tryYield queueId state env
forceRunFunApplication queueId (FFunApplicationB "make_semaphore" []) state env = do
    let (SemaphoreT [] 0 semId, newState) = getNewSemaphore state
    return (FSemaphore semId, newState, env)
forceRunFunApplication queueId (FFunApplicationB "v" [semref]) state env = do
    (newState, FSemaphore semId) <- runVS queueId semref env state
    let (SemaphoreT blockedQueues semValue _) = getSemaphore semId newState
    if null blockedQueues
        then do
            let newerState = putSemaphore semId (SemaphoreT [] (semValue + 1) semId) newState
            return (FTValueStatement [], newerState, env)
        else do
            let newerState = putSemaphore semId (SemaphoreT (cutLast blockedQueues) semValue semId) newState
            return (FTValueStatement [], newerState, env)
forceRunFunApplication queueId a@(FFunApplicationB name args) state env = do
    (s, vs) <- runVS queueId (FAValueStatement a) env state
    return (vs, s, env)
forceRunFunApplication queueId a@(FFunApplicationR loc [args]) state env = do
    (s, vs) <- runVS queueId (FAValueStatement a) env state
    return (vs, s, env)
forceRunFunApplication queueId a _ _ = traceD ("forceRunFunApplication " ++ show a) undefined

runUntilSemReady :: Int -> Int -> S -> E -> IO (FValueStatement, S, E)
runUntilSemReady queueId semId state env = do
    printD $ "runUnitlSemReady " ++ show queueId
    if anyAvailibleQueue state
        then do
            let availibleQueue = getAvailibleQueue state
            newState <- runQueue availibleQueue state
            let queue = getQueue queueId newState
            if checkNotBlocked queue newState
                then return (FTValueStatement [], newState, env)
                else runUntilSemReady queueId semId state env
        else fail "Deadlock"

tryYield :: Int -> S -> E -> IO (FValueStatement, S, E)
tryYield queueId state env = do
    printD $ "tryYield queueId = " ++ show queueId
    let newState = yieldQueue queueId state
    if anyAvailibleQueue newState
        then do
            let availibleQueue = getAvailibleQueue newState
            newerState <- runQueue availibleQueue newState
            let newestState = unyieldQueue queueId newerState
            return (FTValueStatement [], newestState, env)
        else do
            printD "nothing to yield to"
            return (FTValueStatement [], state, env)