module Run.RunVS where

import StaticCheck.Format
import Util.Util
import Util.Env
import Util.State
import Run.OneStepEvaluation
import Run.WrapFunction

runQueue :: QueueT -> S -> IO S
runQueue (QueueT env vs queueId _ _) state = do
    printD $ "running queue with id " ++ show queueId
    (ns, _, nvs) <- runVS queueId vs env state
    return $ putInQueue queueId (QueueT env nvs queueId True False) ns

interpretVS :: Int -> FValueStatement -> E -> [FPatternMatch] -> Int -> S -> [FValueStatement] -> IO (S, FValueStatement)
interpretVS queueId vs env argNames loc s vss =
    if length vss < length argNames
        then do
            let (s_, vs) = wrapFunction vs argNames vss env s
            (nns, _, nnvs) <- runVS queueId vs env s_
            return (nns, nnvs)
        else do
            (newEnv, newState) <- registerArgs env s argNames vss
            (ns, _, nvs) <- runVS queueId vs newEnv newState
            let (b, t, e, _) = stateLookup loc ns
            let nns = putInLoc loc (b, t, e, nvs) ns
            return (nns, nvs)

runSpecialFunction :: Int -> FValueStatement -> E -> S -> IO (S, E, FValueStatement)
runSpecialFunction queueId (FAValueStatement ap@(FFunApplicationB x r)) e s = do
    (nvs, ns, e) <- forceRunFunApplication queueId ap s e
    return (ns, e, nvs)
runSpecialFunction _ a _ _ = traceD (show a) undefined

checkSpecialFunctionName :: String -> Bool
checkSpecialFunctionName funName =
    funName `elem` ["print", "get", "set", "v", "p", "make_semaphore", "yield"]

isLambda (FFValueStatement _ _) = True
isLambda _ = False

runLambda :: Int -> FValueStatement -> [FValueStatement] -> E -> S -> IO (S, E, FValueStatement)
runLambda queueId (FFValueStatement argName vs) (argVs:argVss) env state = do
    let (newLoc, newState) = getNewLoc state
    let newEnv = registerLoc False env argName newLoc
    let newerState = putInLoc newLoc (True, newEnv, FTypeT [], argVs) newState
    runLambda queueId vs argVss newEnv newerState
runLambda queueId vs [] env state = return (state, env, vs)
runLambda queueId vs args env state = do
    (newState, newEnv, nvs) <- runVS queueId vs env state
    if isLambda nvs
        then runLambda queueId nvs args newEnv newState
        else undefined -- infinite loop

runLambdaWrapper :: Int -> [Int] -> [FValueStatement] -> E -> S -> IO (S, E, FValueStatement)
runLambdaWrapper _ [] _ _ _ = undefined
runLambdaWrapper queueId (loc:locs) vss env state = do
    let argPMs = funArgNamesLookup state loc
    fits <- fitPatternMatchs state env argPMs vss
    if fits
        then do
            let (_, _, _, vs) = stateLookup loc state
            let argVSs = take (length argPMs) vss
            let restVSs = takeLast (length vss - length argPMs) vss
            (newEnv, newState) <- registerArgs env state argPMs argVSs
            runLambda queueId vs restVSs newEnv newState
        else 
            runLambdaWrapper queueId locs vss env state

runNotSpecialFunction :: Int -> FValueStatement -> E -> S -> IO (S, E, FValueStatement)
runNotSpecialFunction queueId a@(FAValueStatement (FFunApplicationB funName funArgVss)) env oldState = do
    let firstLoc = lookupFirstLoc funName env
    let locs = lookupLoc funName env
    let funArgNames = funArgNamesLookup oldState firstLoc
    let (ifFunction, _, _, vs) = stateLookup firstLoc oldState
    (state, almostComputedVss) <- foldl (runVSInFoldF env) (return (oldState, [])) funArgVss
    let computedVss = reverse almostComputedVss
    if isLambda vs 
        then do 
            (s, e, nvs) <- runLambdaWrapper queueId locs computedVss env oldState
            runVS queueId nvs e s
        else
            if length computedVss < length funArgNames
        then do
            (nnnState, wrappedFunctionVS) <- wrapFunctionB a env oldState
            runVS queueId wrappedFunctionVS env nnnState
        else do
            (e, updatedVS, b, pms) <- appendFAVS locs computedVss state
            let tooManyAppliedResult = if b
                then do
                        (newEnv, newState) <- registerArgs e state pms computedVss
                        runVS queueId updatedVS newEnv newState
                else
                    runVS queueId updatedVS e state
            if ifFunction
                then
                    if length computedVss > length funArgNames
                        then tooManyAppliedResult
                        else tryRunVSFunApplR queueId locs computedVss state e
                else if length computedVss > length funArgNames
                    then tooManyAppliedResult
                    else do
                        (nnns, nnnvs) <- interpretVS queueId vs env funArgNames firstLoc oldState funArgVss
                        return (nnns, env, nnnvs)

runVS :: Int -> FValueStatement -> E -> S -> IO (S, E, FValueStatement)
runVS queueId vs@FForceValueStatement{} = runVSForceLet queueId vs
runVS queueId vs@FIValueStatement{} = runVSI queueId vs
runVS queueId vs@(FAValueStatement FFunApplicationB{}) = runVSFunB queueId vs
runVS queueId vs@(FExpr FEMul{}) = runVSMul queueId vs
runVS queueId vs@FIfValueStatement{} = runVSIf queueId vs
runVS queueId vs@(FExpr FEEQ{}) = runVSEQ queueId vs
runVS queueId vs@(FExpr FESub{}) = runVSSub queueId vs
runVS queueId vs@(FExpr FEAdd{}) = runVSAdd queueId vs
runVS queueId vs@(FAValueStatement FFunApplicationR{}) = runVSFunR queueId vs
runVS queueId vs@FLitStrValueStatement{} = runVSStr queueId vs
runVS queueId vs@FCValueStatement{} = runVSC queueId vs
runVS queueId vs@FTValueStatement{} = runVST queueId vs
runVS queueId vs@FRefAddr{} = runVSRef queueId vs
runVS queueId vs@FFValueStatement{} = runVSLam queueId vs
runVS queueId vs@FSusValueStatement{} = runVSSusSt queueId vs
runVS queueId vs@FSuspendedValue{} = runVSSusVal queueId vs
runVS queueId vs@FSemaphore{} = runVSSem queueId vs
runVS queueId vs@(FValueStatementB _ _) = runVSLazyLet queueId vs
runVS _ vs = undefined

runVSMul :: Int -> FValueStatement -> E -> S -> IO (S, E, FValueStatement) 
runVSMul queueId (FExpr (FEMul vs1 vs2)) env state = do
    (newState, _, FIValueStatement i1) <- runVS queueId vs1 env state
    (newerState, _, FIValueStatement i2) <- runVS queueId vs2 env newState 
    return (newerState, env, FIValueStatement $ i1 * i2)

runVSEQ :: Int -> FValueStatement -> E -> S -> IO (S, E, FValueStatement)
runVSEQ queueId (FExpr (FEEQ vs1 vs2)) env state = do
    (newState, _, FIValueStatement i1) <- runVS queueId vs1 env state
    (newerState, _, FIValueStatement i2) <- runVS queueId vs2 env newState
    return (newerState, env, FIValueStatement $ if i1 == i2 then 1 else 0)

runVSSub :: Int -> FValueStatement -> E -> S -> IO (S, E, FValueStatement)
runVSSub queueId (FExpr (FESub vs1 vs2)) env state = do
    (newState, _, FIValueStatement i1) <- runVS queueId vs1 env state
    (newerState, _, FIValueStatement i2) <- runVS queueId vs2 env newState
    return (newerState, env, FIValueStatement (i1 - i2))

runVSForceLet :: Int -> FValueStatement -> E -> S -> IO (S, E, FValueStatement)
runVSForceLet queueId (FForceValueStatement assignments vs) env state = do
    (newState, newEnv) <- forceRegisterAssignments queueId assignments state env
    seq newState $ runVS queueId vs newEnv newState

runVSI :: Int -> FValueStatement -> E -> S -> IO (S, E, FValueStatement)
runVSI queueId vs@(FIValueStatement i) e s = return (s, e, vs)

runVSIf :: Int -> FValueStatement -> E -> S -> IO (S, E, FValueStatement)
runVSIf queueId (FIfValueStatement condvs res1vs res2vs) env state = do
    (newState, _, FIValueStatement condVal) <- runVS queueId condvs env state
    if condVal /= 0
        then runVS queueId res1vs env newState
        else runVS queueId res2vs env newState

runVSFunB :: Int -> FValueStatement -> E -> S -> IO (S, E, FValueStatement)
runVSFunB queueId a@(FAValueStatement (FFunApplicationB funName _)) =
    if checkSpecialFunctionName funName
        then
            runSpecialFunction queueId a
        else 
            runNotSpecialFunction queueId a

runVSAdd :: Int -> FValueStatement -> E -> S -> IO (S, E, FValueStatement)
runVSAdd queueId vs@(FExpr (FEAdd vs1 vs2)) env state = do
    (newState, _, FIValueStatement i1) <- runVS queueId vs1 env state
    (newerState, _, FIValueStatement i2) <- runVS queueId vs2 env newState
    return (newerState, env, FIValueStatement (i1 + i2))
    
runVSFunR :: Int -> FValueStatement -> E -> S -> IO (S, E, FValueStatement)
runVSFunR queueId (FAValueStatement (FFunApplicationR locs args)) env state = 
    tryRunVSFunApplR queueId locs args state env

tryRunVSFunApplR :: Int -> [Int] -> [FValueStatement] -> S -> E -> IO (S, E, FValueStatement)
    -- todo: here might fail
tryRunVSFunApplR _ [] _ _ _ = undefined
-- todo: hard to find the function name, probably unneeded, only one at the time
tryRunVSFunApplR queueId (x:xs) args state env = do
    let (ifFunction, env, _, vs) = stateLookup x state
    let argNames = funArgNamesLookup state x
    fits <- fitPatternMatchs state env argNames args
    if fits
        then
            if length args > length argNames
                then do
                    (e, updatedVS, b, pms) <- appendFAVS [x] args state
                    if b
                        then do
                            (newEnv, newState) <- registerArgs e state pms args
                            runVS queueId updatedVS newEnv newState
                        else 
                            runVS queueId updatedVS e state
                else do
                    (newEnv, newState) <- registerArgs env state argNames args
                    runVS queueId vs newEnv newState
        else 
            tryRunVSFunApplR queueId xs args state env


runVSStr :: Int -> FValueStatement -> E -> S -> IO (S, E, FValueStatement)
runVSStr queueIdStr vs@(FLitStrValueStatement _) e s = return (s, e, vs)

runVSC :: Int -> FValueStatement -> E -> S -> IO (S, E, FValueStatement)
runVSC queueId (FCValueStatement name vss) e s = do
    (new_state, newVss) <- foldl (cvsInFoldF queueId e) (return (s, [])) vss
    return (new_state, e, FCValueStatement name (reverse newVss))

cvsInFoldF :: Int -> E -> IO (S, [FValueStatement]) -> FValueStatement -> IO (S, [FValueStatement])
cvsInFoldF queueId e acc vs = do
    (state, vsl) <- acc
    (new_state, _, vs_) <- runVS queueId vs e state
    return (new_state, vs_:vsl)

runVST :: Int -> FValueStatement -> E -> S -> IO (S, E, FValueStatement)
runVST queueId (FTValueStatement xss) e s = do
    (ns, xssc) <- foldl (runTVSInFoldF queueId e) (return (s, [])) xss
    return (ns, e, FTValueStatement xssc)

runTVSInFoldF :: Int -> E -> IO (S, [FValueStatement]) -> FValueStatement -> IO (S, [FValueStatement])
runTVSInFoldF queueId env acc vs = do
    (s, vsl) <- acc
    (ns, _, nvs) <- runVS queueId vs env s
    return (ns, nvs:vsl)

runVSLam :: Int -> FValueStatement -> E -> S -> IO (S, E, FValueStatement)
runVSLam queueId a@(FFValueStatement argName vs) e s =
    return (s, e, a)

runVSSusSt :: Int -> FValueStatement -> E -> S -> IO (S, E, FValueStatement)
runVSSusSt queueId (FSusValueStatement vs) e s = do
    let queueId = getFreeQueueId s
    let nvs = FSuspendedValue queueId
    let ns = putQueue s (QueueT e vs queueId False False)
    return (ns, e, nvs)

runVSSusVal :: Int -> FValueStatement -> E -> S -> IO (S, E, FValueStatement)
runVSSusVal queueId (FSuspendedValue qId) e s = do
    let (QueueT env qvs _ b y) = getQueue qId s
    (ns, _, nvs) <- runVS qId qvs env s
    let nns = putInQueue qId (QueueT env qvs qId True False) ns
    return (nns, e, nvs)

runVSSem :: Int -> FValueStatement -> E -> S -> IO (S, E, FValueStatement)
runVSSem queueId vs@(FSemaphore i) e s =
    return (s, e, vs)

runVSRef :: Int -> FValueStatement -> E -> S -> IO (S, E, FValueStatement)
runVSRef queueId vs@(FRefAddr x) e s = return (s, e, vs)

runVSLazyLet :: Int -> FValueStatement -> E -> S -> IO (S, E, FValueStatement)
runVSLazyLet queueId (FValueStatementB assignments vs) env state = do
    (newState, newEnv) <- lazyRegisterAssignments queueId assignments state env
    seq newState $ runVS queueId vs newEnv newState
 

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

forceRegisterAssignments :: Int -> [FAssignment] -> S -> E -> IO (S, E)
forceRegisterAssignments queueId assignments state env =
    foldl (forceRegisterAssignmentsInFoldF queueId) (return (state, env)) assignments

lazyRegisterAssignments :: Int -> [FAssignment] -> S -> E -> IO (S, E)
lazyRegisterAssignments queueId assignments state env =
    foldl (lazyRegisterAssignmentsInFoldF queueId) (return (state, env)) assignments

forceRegisterAssignmentsInFoldF :: Int -> IO (S, E) -> FAssignment -> IO (S, E)
forceRegisterAssignmentsInFoldF queueId acc assignment = do
    (s, e) <- acc
    forceRegisterAssignment queueId assignment s e

lazyRegisterAssignmentsInFoldF :: Int -> IO (S, E) -> FAssignment -> IO (S, E)
lazyRegisterAssignmentsInFoldF queueId acc assignment = do
    (s, e) <- acc
    lazyRegisterAssignment queueId assignment s e

forceRegisterAssignment :: Int -> FAssignment -> S -> E -> IO (S, E)
forceRegisterAssignment queueId a@(FAssignmentB typ@(FTypeB "Ref" [t]) (FPatternMatchB name) vs) state env = do
    let (refLoc, newState) = getNewLoc state
    let (vsLoc, newerState) = getNewLoc newState
    let refVS = FRefAddr vsLoc
    let newEnv = registerLoc False env name refLoc
    let newererState = putInLoc refLoc (False, newEnv, typ, refVS) newerState
    (newerererState, _, nvs) <- runVS queueId vs newEnv newererState
    let newererererState = putInLoc vsLoc (False, newEnv, t, nvs) newerererState
    return (newererererState, newEnv)
forceRegisterAssignment queueId a@(FAssignmentB t pm vs) state env = do
    setPM queueId t pm vs state env
forceRegisterAssignment queueId a _ _ = traceD (show a) undefined

lazyRegisterAssignment :: Int -> FAssignment -> S -> E -> IO (S, E)
lazyRegisterAssignment queueId (FAssignmentB t pm vs) state env =
    setPMLazy queueId t pm vs state env
lazyRegisterAssignment _ _ _ _ = undefined

setPM :: Int -> FType -> FPatternMatch -> FValueStatement -> S -> E -> IO (S, E)
setPM qId (FTypeT types) (FPatternMatchT pmL) vs state env = do
    (vss, newState, newEnv, _) <- forceGetTupleVSS qId vs state env
    foldl (setPMInFoldF qId) (return (newState, newEnv)) $ tList types pmL vss
setPM qId t (FPatternMatchB x) vs@(FAValueStatement _) state env = do
    let (loc, newState) = getNewLoc state
    let newEnv = registerLoc False env x loc
    (newerState, _, nvs) <- runVS qId vs newEnv newState
    let newererState = putInLoc loc (False, newEnv, t, nvs) newerState
    return (newererState, newEnv)
setPM qId t (FPatternMatchB x) vs state env = do
    let (loc, newState) = getNewLoc state
    let newEnv = registerLoc False env x loc
    (newerState, nvs) <- interpretVS qId vs newEnv [] loc newState []
    let newererState = putInLoc loc (False, newEnv, t, nvs) newerState
    return (newererState, newEnv)
setPM _ _ _ _ _ _ = undefined

setPMLazy :: Int -> FType -> FPatternMatch -> FValueStatement -> S -> E -> IO (S, E)
setPMLazy qId (FTypeT types) (FPatternMatchT pmL) vs state env = undefined
setPMLazy qId t (FPatternMatchB x) vs state env = do
    let (loc, newState) = getNewLoc state
    let newEnv = registerLoc False env x loc
    let newerState = putInLoc loc (False, newEnv, t, vs) newState
    return (newerState, newEnv)

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
    (ns, _, tvs) <- runVS queueId a e s
    return (unwrapSingleTuples tvs, ns, e, False)
forceGetTupleVSS queueId a@(FAValueStatement funApplication) state env = do
    (vs, s, e) <- forceRunFunApplication queueId funApplication state env
    forceGetTupleVSS queueId vs s e
forceGetTupleVSS qId vs@(FSuspendedValue queueId) s e =
    return ([vs], s, e, False)
forceGetTupleVSS _ vs _ _ = traceD vs undefined

forceRunFunApplication :: Int -> FFunApplication -> S -> E -> IO (FValueStatement, S, E)
forceRunFunApplication queueId (FFunApplicationB "print" [str]) state env = do
    (s, _, r) <- runVS queueId str env state
    case r of
        FSuspendedValue qId -> do
            (ns, _, nr) <- runVS qId r env s
            print nr
            return (FTValueStatement [], ns, env)
        _ -> do
            print r
            return (FTValueStatement [], s, env)
forceRunFunApplication queueId (FFunApplicationB "set" [ref, value]) state env = do
    (newState, _, FRefAddr refAddr) <- runVS queueId ref env state
    let newerState = putInLoc refAddr (False, env, FTypeT [], value) newState
    return (FTValueStatement [], newerState, env)
forceRunFunApplication queueId (FFunApplicationB "get" [ref]) state env = do
    (newState, _, FRefAddr refAddr) <- runVS queueId ref env state
    let (_, e, _, refVS) = stateLookup refAddr newState
    (newerState, _, vs) <- runVS queueId refVS e newState
    return (vs, newerState, e)
forceRunFunApplication queueId (FFunApplicationB "p" [semref]) state env = do
    (newState, _, FSemaphore semId) <- runVS queueId semref env state
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
    (newState, _, FSemaphore semId) <- runVS queueId semref env state
    let (SemaphoreT blockedQueues semValue _) = getSemaphore semId newState
    if null blockedQueues
        then do
            let newerState = putSemaphore semId (SemaphoreT [] (semValue + 1) semId) newState
            return (FTValueStatement [], newerState, env)
        else do
            let newerState = putSemaphore semId (SemaphoreT (cutLast blockedQueues) semValue semId) newState
            forceRunUnit newerState env
forceRunFunApplication queueId a@(FFunApplicationB name args) state env = do
    (s, _, vs) <- runVS queueId (FAValueStatement a) env state
    return (vs, s, env)
forceRunFunApplication queueId a@(FFunApplicationR loc [args]) state env = do
    (s, _, vs) <- runVS queueId (FAValueStatement a) env state
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
                then forceRunUnit newState env
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
            forceRunUnit newestState env
        else do
            printD "nothing to yield to"
            forceRunUnit state env
        
forceRunUnit :: S -> E -> IO (FValueStatement, S, E)
forceRunUnit s e = return (FTValueStatement [], s, e)