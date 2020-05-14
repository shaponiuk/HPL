module Run.RunVS where

import StaticCheck.Format
import Util.Util
import Util.Env
import Util.State
import Control.Monad

runQueue :: QueueT -> S -> IO S
runQueue (QueueT env vs queueId _ _) state = do
    printD $ "running queue with id " ++ show queueId
    (ns, _, nvs) <- runVS queueId vs env state
    return $ putInQueue queueId (QueueT env nvs queueId True False) ns

interpretVS :: Int -> FValueStatement -> E -> [FPatternMatch] -> Int -> S -> [FValueStatement] -> IO (S, E, FValueStatement)
interpretVS queueId vs env argNames loc s vss =
    if length vss < length argNames
        then do
            let (s_, vs) = wrapFunction vs argNames vss env s
            runVS queueId vs env s_
        else do
            -- when (loc == 11) $ printD loc >> printD vs
            (newEnv, newState) <- registerArgs queueId env s argNames vss
            (ns, ne, nvs) <- runVS queueId vs newEnv newState
            let (b, ne, t, _) = stateLookup loc ns
            let nns = putInLoc loc (b, ne, t, nvs) ns
            printD "HAHAHA"
            printD loc
            printD ne
            printD nvs
            return (nns, ne, nvs)

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
    printD "heeereee"
    -- printD vs
    let nvs = convertBApplicationsToRApplications vs newEnv
    -- printD nvs
    runLambda queueId nvs argVss newEnv newerState
runLambda queueId vs [] env state = do
    let nvs = convertBApplicationsToRApplications vs env
    return (state, env, nvs)
runLambda queueId vs args env state = do
    (newState, newEnv, nvs) <- runVS queueId vs env state
    if isLambda nvs
        then runLambda queueId nvs args newEnv newState
        else undefined -- infinite loop

runLambdaWrapper :: Int -> [Int] -> [FValueStatement] -> E -> S -> IO (S, E, FValueStatement)
runLambdaWrapper _ [] _ _ _ = undefined
runLambdaWrapper queueId (loc:locs) vss env state = do
    let argPMs = funArgNamesLookup state loc
    (fits, state, vss, env) <- fitPatternMatchs queueId state env argPMs vss
    if fits
        then do
            let (_, innerEnv, _, vs) = stateLookup loc state
            let argVSs = take (length argPMs) vss
            let restVSs = takeLast (length vss - length argPMs) vss
            (innerEnv, newState) <- registerArgs queueId innerEnv state argPMs argVSs
            runLambda queueId vs restVSs innerEnv newState
        else 
            runLambdaWrapper queueId locs vss env state

runNotSpecialFunction :: Int -> FValueStatement -> E -> S -> IO (S, E, FValueStatement)
runNotSpecialFunction queueId a@(FAValueStatement (FFunApplicationB funName funArgVss_)) outerEnv state = do
    let firstLoc = lookupFirstLoc funName outerEnv
    let locs = lookupLoc funName outerEnv
    let funArgNames = funArgNamesLookup state firstLoc
    let (ifFunction, innerEnv, _, vs) = stateLookup firstLoc state
    -- when (funName == "x") $ printD funName >> printD vs >> printD firstLoc >> printD funArgNames
    (state, funArgVss_) <- foldl (runVSInFoldF outerEnv) (return (state, [])) funArgVss_
    let funArgVss = reverse funArgVss_
    if isLambda vs 
        then do 
            (s, e, nvs) <- runLambdaWrapper queueId locs funArgVss innerEnv state
            runVS queueId nvs e s
        else
            if length funArgVss < length funArgNames
                then do
                    (state, vs) <- wrapFunctionB queueId a innerEnv state
                    printD "wrapped"
                    printD vs
                    return (state, innerEnv, vs)
                else do
                    let tooManyAppliedResult = do
                        (innerEnv, favsAppend, b, pms, state) <- appendFAVS queueId locs funArgVss state
                        if b
                            then do
                                (innerEnv, state) <- registerArgs queueId innerEnv state pms funArgVss
                                runVS queueId favsAppend innerEnv state
                            else
                                runVS queueId favsAppend innerEnv state
                    if ifFunction
                        then
                            if length funArgVss > length funArgNames
                                then tooManyAppliedResult
                                else do
                                    -- when (funName == "first") $ printD funName >> printD funArgVss >> printD funArgNames >> printD locs
                                    tryRunVSFunApplR queueId locs funArgVss state innerEnv
                        else if length funArgVss > length funArgNames
                            then tooManyAppliedResult
                            else do
                                -- when (funName == "a") $ printD funName >> printD vs >> printD firstLoc
                                (state, innerEnv, vs) <- interpretVS queueId vs innerEnv funArgNames firstLoc state funArgVss
                                return (state, innerEnv, vs)

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
runVS queueId vs@FValueStatementB{} = runVSLazyLet queueId vs
runVS _ vs = traceD vs undefined

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
    printD condvs >> printD env
    (newState, _, FIValueStatement condVal) <- runVS queueId condvs env state
    printD "jfdklas;jfkdsla;jkadsl"
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
tryRunVSFunApplR queueId (x:xs) args_ state outerEnv = do
    let (ifFunction, innerEnv, _, vs) = stateLookup x state
    let argNames = funArgNamesLookup state x
    (fits, state, args_, outerEnv) <- fitPatternMatchs queueId state outerEnv argNames args_
    let args = convertBApplicationsToRApplicationsM args_ outerEnv
    when (x == 7) $ printD x
    if fits
        then
            if length args > length argNames
                then do
                    (innerEnv, updatedVS, b, pms, state) <- appendFAVS queueId [x] args state
                    if b
                        then do
                            (innerEnv, state) <- registerArgs queueId innerEnv state pms args
                            runVS queueId updatedVS innerEnv state
                        else 
                            runVS queueId updatedVS innerEnv state
                else
                    if length args < length argNames then undefined
                        else do
                            (innerEnv, state) <- registerArgs queueId innerEnv state argNames args
                            -- when (x == 2) $ printD x >> printD args >> printD argNames >> printD vs >> printD innerEnv
                            -- when (x == 4) $ printD x >> printD vs >> printD ifFunction >> printD argNames >> printD args >> printD fits >> printD innerEnv
                            -- when (x == 10) $ printD x >> printD vs >> printD innerEnv
                            runVS queueId vs innerEnv state
        else 
            tryRunVSFunApplR queueId xs args state outerEnv


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

convertBApplicationsToRApplicationsM :: [FValueStatement] -> E -> [FValueStatement]
convertBApplicationsToRApplicationsM [] _ = []
convertBApplicationsToRApplicationsM (x:xs) e =
    convertBApplicationsToRApplications x e : convertBApplicationsToRApplicationsM xs e

convertBApplicationsToRApplications :: FValueStatement -> E -> FValueStatement
convertBApplicationsToRApplications (FExpr (FESub vs1 vs2)) e =
    FExpr $ FESub nvs1 nvs2 where
        nvs1 = convertBApplicationsToRApplications vs1 e
        nvs2 = convertBApplicationsToRApplications vs2 e
convertBApplicationsToRApplications (FAValueStatement (FFunApplicationB funName funArgs)) e =
    FAValueStatement $ FFunApplicationR locs (convertBApplicationsToRApplicationsM funArgs e) where
        locs = lookupLoc funName e
convertBApplicationsToRApplications a@(FIValueStatement _) _ = a
convertBApplicationsToRApplications (FCValueStatement name vss) env =
    FCValueStatement name $ map (`convertBApplicationsToRApplications` env) vss
convertBApplicationsToRApplications (FTValueStatement vss) env =
    FTValueStatement $ map (`convertBApplicationsToRApplications` env) vss
convertBApplicationsToRApplications (FExpr (FEMul vs1 vs2)) env =
    FExpr $ FEMul (convertBApplicationsToRApplications vs1 env) (convertBApplicationsToRApplications vs2 env)
convertBApplicationsToRApplications (FAValueStatement (FFunApplicationR locs argVss)) e =
    FAValueStatement $ FFunApplicationR locs $ convertBApplicationsToRApplicationsM argVss e
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
setPM qId t (FPatternMatchB x) vs@(FAValueStatement _) state__ env_ = do
    printD "setPM" >> printD x >> printD env_ >> printD vs
    let (loc, state_) = getNewLoc state__
    let env = registerLoc False env_ x loc
    (state_, inenv, nvs) <- runVS qId vs env state_
    let state = putInLoc loc (False, inenv, t, nvs) state_
    return (state, env)
setPM qId t (FPatternMatchB x) vs state env = do
    let (loc, newState) = getNewLoc state
    let newEnv = registerLoc False env x loc
    (newerState, newEnv, nvs) <- interpretVS qId vs newEnv [] loc newState []
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

oneStepEvaluation :: Int -> S -> E -> FValueStatement -> IO (S, E, FValueStatement)
oneStepEvaluation queueId s e (FAValueStatement (FFunApplicationR locs argVss)) = oneStepTryRunVSFunApplR queueId locs argVss s
oneStepEvaluation queueId s e (FAValueStatement (FFunApplicationB name vss)) = do
    let locs = lookupLoc name e
    oneStepTryRunVSFunApplR queueId locs vss s
oneStepEvaluation queueId s e (FForceValueStatement assignments vs) = do
    (ns, ne) <- forceRegisterAssignments queueId assignments s e
    return (ns, ne, vs)
oneStepEvaluation queueId s e vs = traceD ("oneStepEvaluation " ++ show vs) undefined

oneStepTryRunVSFunApplR :: Int -> [Int] -> [FValueStatement] -> S -> IO (S, E, FValueStatement)
oneStepTryRunVSFunApplR _ [] _ _ = undefined
-- todo: hard to find the function name, probably unneeded, only one at the time
oneStepTryRunVSFunApplR queueId (x:xs) args state = do
    let (ifFunction, env, _, vs) = stateLookup x state
    let argNames = funArgNamesLookup state x
    (fits, state, args, env) <- fitPatternMatchs queueId state env argNames args
    if fits
        then
            if length args > length argNames
                then do
                    (e, updatedVS, b, pms, state) <- appendFAVS queueId [x] args state
                    if b
                        then
                            getOneStepRegisterTriple queueId e state pms args updatedVS
                        else
                            return (state, e, updatedVS)
                else if length args == length argNames
                    then
                        getOneStepRegisterTriple queueId env state argNames args vs
                    else
                        undefined
        else 
            oneStepTryRunVSFunApplR queueId xs args state

getOneStepRegisterTriple :: Int -> E -> S -> [FPatternMatch] -> [FValueStatement] -> FValueStatement -> IO (S, E, FValueStatement)
getOneStepRegisterTriple queueId e state pms args vs = do
    (newEnv, newState) <- registerArgs queueId e state pms args
    return (newState, newEnv, vs)

registerArgs :: Int -> E -> S -> [FPatternMatch] -> [FValueStatement] -> IO (E, S)
registerArgs queueId env state argNames vss = 
    foldl (registerArgsInFoldF queueId) (return (env, state)) $ dList argNames vss

registerArgsInFoldF :: Int -> IO (E, S) -> (FPatternMatch, FValueStatement) -> IO (E, S)
registerArgsInFoldF queueId acc (FPatternMatchB str, vs) = do
    (e, s) <- acc
    let (newLoc, newState) = getNewLoc s
    let newEnv = registerLoc False e str newLoc
    let newerState = putInLoc newLoc (False, newEnv, FTypeT [], vs) newState
    return (newEnv, newerState)
registerArgsInFoldF queueId acc (FPatternMatchI _, _) = acc
registerArgsInFoldF queueId acc (FPatternMatchC _ pms, FCValueStatement _ vss) = do
    (e, s) <- acc
    registerArgs queueId e s pms vss
registerArgsInFoldF queueId acc (FPatternMatchC cname pms, FAValueStatement (FFunApplicationR loc argVss)) = do
    (e, s) <- acc
    (ns, ne, nvs) <- oneStepEvaluation queueId s e $ FAValueStatement (FFunApplicationR loc argVss)
    registerArgsInFoldF queueId (return (ne, ns)) (FPatternMatchC cname pms, nvs)
registerArgsInFoldF queueId acc (pm@FPatternMatchT{}, vs@FAValueStatement{}) = registerArgsInFoldFOneStep queueId acc (pm, vs)
registerArgsInFoldF queueId acc (pm@FPatternMatchT{}, vs@FForceValueStatement{}) = registerArgsInFoldFOneStep queueId acc (pm, vs)
registerArgsInFoldF queueId acc a@(FPatternMatchT t1, FTValueStatement t2) = do
    (e, s) <- acc
    registerArgs queueId e s t1 t2
registerArgsInFoldF queueId acc a@(FPatternMatchS loc, vs@FIValueStatement{}) = do
    (e, s) <- acc
    let newState = putInLoc loc (False, e, FTypeT [], vs) s
    return (e, newState)
registerArgsInFoldF _ _ el = traceD ("registerArgsInFoldF " ++ show el) undefined

registerArgsInFoldFOneStep queueId acc (pm, vs) = do
    (e, s) <- acc
    (ns, ne, nvs) <- oneStepEvaluation queueId s e vs
    registerArgsInFoldF queueId (return (ne, ns)) (pm, nvs)

appendFAVS :: Int -> [Int] -> [FValueStatement] -> S -> IO (E, FValueStatement, Bool, [FPatternMatch], S)
appendFAVS _ [] vss state = fail "Non exhaustive pattern matches"
appendFAVS queueId (loc:xs) addVss state = do
    let argNames = funArgNamesLookup state loc
    let (f, e, _, vs) = stateLookup loc state
    (fits, newState, addVss, e) <- fitPatternMatchs queueId state e argNames addVss
    if fits
        then do
            let (appendedVS, b, pms) = appendFAVSInt vs addVss
            return (e, appendedVS, b, pms, newState)
        else 
            appendFAVS queueId xs addVss newState

appendFAVSInt :: FValueStatement -> [FValueStatement] -> (FValueStatement, Bool, [FPatternMatch])
appendFAVSInt (FAValueStatement (FFunApplicationB funName vss)) addVss = (FAValueStatement $ FFunApplicationB funName (vss ++ addVss), False, [])
appendFAVSInt (FFValueStatement name vs) addVSS = (vs, True, [FPatternMatchB name])
appendFAVSInt (FAValueStatement (FFunApplicationR locs vss)) addVss = (FAValueStatement $ FFunApplicationR locs (vss ++ addVss), False, [])
appendFAVSInt x y = traceD ("appendFAVSInt " ++ show x ++ "\n" ++ show y) undefined

fitPatternMatchs :: Int -> S -> E -> [FPatternMatch] -> [FValueStatement] -> IO (Bool, S, [FValueStatement], E)
fitPatternMatchs _ s e _ [] = return (True, s, [], e)
fitPatternMatchs _ s e [] vss = return (True, s, vss, e)
fitPatternMatchs queueId s e (pm:pms) (vs:vss) = do
    (b, ns, vs, e) <- fitPatternMatch queueId s e (pm, vs)
    if b 
        then do
            (b__, s__, vss__, e) <- fitPatternMatchs queueId ns e pms vss
            return (b__, s__, vs:vss__, e)
        else return (False, ns, vs:vss, e)

fitPatternMatch :: Int -> S -> E -> (FPatternMatch, FValueStatement) -> IO (Bool, S, FValueStatement, E)
fitPatternMatch _ s e (FPatternMatchI i1, vs@(FIValueStatement i2)) = return (i1 == i2, s, vs, e)
fitPatternMatch queueId s e a@(FPatternMatchC (FPatternMatchB name1) pms, FCValueStatement name2 vss) = do
    (res1, ns, nvss, e) <- fitPatternMatchs queueId s e pms vss
    return (name1 == name2 && res1, ns, FCValueStatement name2 nvss, e)
fitPatternMatch queueId s e (FPatternMatchB _, vs) = return (True, s, vs, e)
fitPatternMatch queueId s e (pm@FPatternMatchC{}, vs@FAValueStatement{}) = do
    (ns, ne, nvs) <- oneStepEvaluation queueId s e vs
    fitPatternMatch queueId ns ne (pm, nvs)
fitPatternMatch queueId s e a@(FPatternMatchT tuple, vs@FAValueStatement{}) = do
    (ns, ne, nvs) <- oneStepEvaluation queueId s e vs
    fitPatternMatch queueId ns ne (FPatternMatchT tuple, nvs)
fitPatternMatch queueId s e a@(FPatternMatchT tuple, vs@FForceValueStatement{}) = do
    (ns, ne, nvs) <- oneStepEvaluation queueId s e vs
    fitPatternMatch queueId ns ne (FPatternMatchT tuple, nvs)
fitPatternMatch queueId s e (FPatternMatchT tuple, FTValueStatement tuple_) = do
    (b, s, vss, e) <- fitPatternMatchs queueId s e tuple tuple_
    return (b, s, FTValueStatement vss, e)
fitPatternMatch queueId s e (pm , FForceValueStatement assignments vs) = do
    (ns, ne) <- forceRegisterAssignments queueId assignments s e
    fitPatternMatch queueId ns ne (pm, vs)
fitPatternMatch _ _ _ a = traceD ("fitPatternMatch undefined " ++ show a) undefined

wrapFunction :: FValueStatement -> [FPatternMatch] -> [FValueStatement] -> E -> S -> (S, FValueStatement)
wrapFunction vs pms vss env s = undefined

wrapFunctionB :: Int -> FValueStatement -> E -> S -> IO (S, FValueStatement)
wrapFunctionB queueId vs@(FAValueStatement (FFunApplicationB x vss)) env state = do
    let locs = lookupLoc x env
    wrapFunctionBInt queueId locs vs env state

wrapFunctionBInt :: Int -> [Int] -> FValueStatement -> E -> S -> IO (S, FValueStatement)
wrapFunctionBInt queueId (loc:locs) vs@(FAValueStatement (FFunApplicationB x vss)) env state = do
    let funArgNames = funArgNamesLookup state loc
    (fits, state, vss, env) <- fitPatternMatchs queueId state env funArgNames vss
    if fits
        then do
            let d = length funArgNames - length vss
            let (newLocs, newState) = getNNewLocs state d
            let newLambdas = map show newLocs
            return (newState, wrapFunctionBIntNNewLambdas d vs newLambdas)
        else 
            wrapFunctionBInt queueId locs vs env state

wrapFunctionBIntNNewLambdas :: Int -> FValueStatement -> [String] -> FValueStatement
wrapFunctionBIntNNewLambdas d (FAValueStatement (FFunApplicationB x vss)) locs = 
    let
        argVss = map makeFunApplicationNoArg locs
    in foldl (flip FFValueStatement) (FAValueStatement (FFunApplicationB x (vss ++ argVss))) locs


makeFunApplicationNoArg :: String -> FValueStatement
makeFunApplicationNoArg x = FAValueStatement (FFunApplicationB x [])