module Run.RunVS where

import StaticCheck.Format
import Util.Util
import Util.Env
import Util.State
import Control.Monad

runQueue :: QueueT -> S -> IO S
runQueue (QueueT env vs queueId _ _ _ _) state = do
    printD $ "running queue with id " ++ show queueId
    (ns, nvs) <- runVS queueId vs env state
    return $ putInQueue queueId (QueueT env nvs queueId True False [] []) ns

runSpecialFunction :: Int -> FValueStatement -> E -> S -> IO (S, FValueStatement)
runSpecialFunction queueId (FAValueStatement ap@(FFunApplicationB x r)) e s = do
    (nvs, ns) <- forceRunFunApplication queueId ap s e
    return (ns, nvs)
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
runLambda queueId vs [] env state = do
    return (state, env, vs)
runLambda queueId vs args env state = do
    (newState, nvs) <- runVS queueId vs env state
    if isLambda nvs
        then runLambda queueId nvs args env newState
        else undefined -- infinite loop

runLambdaWrapper :: Int -> [Int] -> [FValueStatement] -> E -> S -> IO (S, E, FValueStatement)
runLambdaWrapper _ [] _ _ _ = undefined
runLambdaWrapper queueId (loc:locs) vss outerEnv state = do
    let argPMs = funArgNamesLookup state loc
    fits <- fitPatternMatchs queueId state outerEnv argPMs vss
    if fits
        then do
            let (_, innerEnv, _, vs) = stateLookup loc state
            let argVSs = take (length argPMs) vss
            let restVSs = takeLast (length vss - length argPMs) vss
            (innerEnv, newState) <- registerArgs queueId innerEnv outerEnv state argPMs argVSs
            runLambda queueId vs restVSs innerEnv newState
        else 
            runLambdaWrapper queueId locs vss outerEnv state

runNotSpecialFunction :: Int -> FValueStatement -> E -> S -> IO (S, FValueStatement)
runNotSpecialFunction queueId a@(FAValueStatement (FFunApplicationB funName funArgVss_)) outerEnv state = do
    let firstLoc = lookupFirstLoc funName outerEnv
    let locs = lookupLoc funName outerEnv
    let funArgNames = funArgNamesLookup state firstLoc
    let (ifFunction, innerEnv, _, vs) = stateLookup firstLoc state
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
                    return (state, vs)
                else do
                    let tooManyAppliedResult = do
                        (innerEnv, favsAppend, b, pms, state) <- appendFAVS queueId locs funArgVss state
                        if b
                            then do
                                (innerEnv, state) <- registerArgs queueId innerEnv outerEnv state pms funArgVss
                                runVS queueId favsAppend innerEnv state
                            else
                                runVS queueId favsAppend innerEnv state
                    if length funArgVss > length funArgNames
                        then tooManyAppliedResult
                        else do
                                -- when (funName == "a") $ printD funName >> printD vs >> printD innerEnv >> printD (stateLookup 15 state)
                                registerArgs queueId innerEnv outerEnv state funArgNames funArgVss
                                runVS queueId vs innerEnv state

runVS :: Int -> FValueStatement -> E -> S -> IO (S, FValueStatement)
runVS queueId vs@FForceValueStatement{} e s = runVSForceLet queueId vs e s
runVS queueId vs@FIValueStatement{} e s = return (s, vs)
runVS queueId vs@(FAValueStatement FFunApplicationB{}) e s = runVSFunB queueId vs e s
runVS queueId vs@(FExpr FEMul{}) e s = runVSMul queueId vs e s
runVS queueId vs@FIfValueStatement{} e s = runVSIf queueId vs e s
runVS queueId vs@(FExpr FEEQ{}) e s = runVSEQ queueId vs e s
runVS queueId vs@(FExpr FESub{}) e s = runVSSub queueId vs e s
runVS queueId vs@(FExpr FEAdd{}) e s = runVSAdd queueId vs e s
runVS queueId vs@(FAValueStatement FFunApplicationR{}) e s = runVSFunR queueId vs e s
runVS queueId vs@FLitStrValueStatement{} e s = runVSStr queueId vs e s
runVS queueId vs@FCValueStatement{} e s = runVSC queueId vs e s
runVS queueId vs@FTValueStatement{} e s = runVST queueId vs e s
runVS queueId vs@FRefAddr{} e s = return (s, vs)
runVS queueId vs@FFValueStatement{} e s = runVSLam queueId vs e s
runVS queueId vs@FSusValueStatement{} e s = runVSSusSt queueId vs e s
runVS queueId vs@FSuspendedValue{} e s = runVSSusVal queueId vs e s
runVS queueId vs@FSemaphore{} e s = return (s, vs)
runVS queueId vs@FValueStatementB{} e s = runVSLazyLet queueId vs e s
runVS _ vs _ _ = traceD vs undefined

runVSMul :: Int -> FValueStatement -> E -> S -> IO (S, FValueStatement) 
runVSMul queueId (FExpr (FEMul vs1 vs2)) env state = do
    (newState, FIValueStatement i1) <- runVS queueId vs1 env state
    (newerState, FIValueStatement i2) <- runVS queueId vs2 env newState 
    return (newerState, FIValueStatement $ i1 * i2)

runVSEQ :: Int -> FValueStatement -> E -> S -> IO (S, FValueStatement)
runVSEQ queueId (FExpr (FEEQ vs1 vs2)) env state = do
    (newState, FIValueStatement i1) <- runVS queueId vs1 env state
    (newerState, FIValueStatement i2) <- runVS queueId vs2 env newState
    return (newerState, FIValueStatement $ if i1 == i2 then 1 else 0)

runVSSub :: Int -> FValueStatement -> E -> S -> IO (S, FValueStatement)
runVSSub queueId (FExpr (FESub vs1 vs2)) env state = do
    (newState, FIValueStatement i1) <- runVS queueId vs1 env state
    (newerState, FIValueStatement i2) <- runVS queueId vs2 env newState
    return (newerState, FIValueStatement (i1 - i2))

runVSForceLet :: Int -> FValueStatement -> E -> S -> IO (S, FValueStatement)
runVSForceLet queueId (FForceValueStatement assignments vs) env state = do
    (newState, newEnv) <- forceRegisterAssignments queueId assignments state env
    seq newState $ runVS queueId vs newEnv newState

runVSIf :: Int -> FValueStatement -> E -> S -> IO (S, FValueStatement)
runVSIf queueId (FIfValueStatement condvs res1vs res2vs) env state = do
    (newState, FIValueStatement condVal) <- runVS queueId condvs env state
    if condVal /= 0
        then runVS queueId res1vs env newState
        else runVS queueId res2vs env newState

runVSFunB :: Int -> FValueStatement -> E -> S -> IO (S, FValueStatement)
runVSFunB queueId a@(FAValueStatement (FFunApplicationB funName _)) =
    if checkSpecialFunctionName funName
        then
            runSpecialFunction queueId a
        else 
            runNotSpecialFunction queueId a

runVSAdd :: Int -> FValueStatement -> E -> S -> IO (S, FValueStatement)
runVSAdd queueId vs@(FExpr (FEAdd vs1 vs2)) env state = do
    (newState, FIValueStatement i1) <- runVS queueId vs1 env state
    (newerState, FIValueStatement i2) <- runVS queueId vs2 env newState
    return (newerState, FIValueStatement (i1 + i2))
    
runVSFunR :: Int -> FValueStatement -> E -> S -> IO (S, FValueStatement)
runVSFunR queueId (FAValueStatement (FFunApplicationR loc)) _ = runLoc queueId loc

runLoc :: Int -> Int -> S -> IO (S, FValueStatement)
runLoc queueId x state = do
    let (_, innerEnv, _, vs) = stateLookup x state
    runVS queueId vs innerEnv state

runVSStr :: Int -> FValueStatement -> E -> S -> IO (S, FValueStatement)
runVSStr queueIdStr vs@(FLitStrValueStatement _) e s = return (s, vs)

runVSC :: Int -> FValueStatement -> E -> S -> IO (S, FValueStatement)
runVSC queueId (FCValueStatement name vss) e s = do
    (new_state, newVss) <- foldl (cvsInFoldF queueId e) (return (s, [])) vss
    return (new_state, FCValueStatement name (reverse newVss))

cvsInFoldF :: Int -> E -> IO (S, [FValueStatement]) -> FValueStatement -> IO (S, [FValueStatement])
cvsInFoldF queueId e acc vs = do
    (state, vsl) <- acc
    (new_state, vs_) <- runVS queueId vs e state
    return (new_state, vs_:vsl)

runVST :: Int -> FValueStatement -> E -> S -> IO (S, FValueStatement)
runVST queueId (FTValueStatement xss) e s = do
    (ns, xssc) <- foldl (runTVSInFoldF queueId e) (return (s, [])) xss
    return (ns, FTValueStatement xssc)

runTVSInFoldF :: Int -> E -> IO (S, [FValueStatement]) -> FValueStatement -> IO (S, [FValueStatement])
runTVSInFoldF queueId env acc vs = do
    (s, vsl) <- acc
    (ns, nvs) <- runVS queueId vs env s
    return (ns, nvs:vsl)

runVSLam :: Int -> FValueStatement -> E -> S -> IO (S, FValueStatement)
runVSLam queueId a@(FFValueStatement argName vs) e s =
    return (s, a)

runVSSusSt :: Int -> FValueStatement -> E -> S -> IO (S, FValueStatement)
runVSSusSt queueId (FSusValueStatement vs) e s = do
    let queueId = getFreeQueueId s
    let nvs = FSuspendedValue queueId
    let ns = putQueue s (QueueT e vs queueId False False [] [])
    return (ns, nvs)

runVSSusVal :: Int -> FValueStatement -> E -> S -> IO (S, FValueStatement)
runVSSusVal queueId (FSuspendedValue qId) e s = do
    let (QueueT env qvs _ b y gq pq) = getQueue qId s
    (ns, nvs) <- runVS qId qvs env s
    let nns = putInQueue qId (QueueT env qvs qId True False gq pq) ns
    return (nns, nvs)

runVSLazyLet :: Int -> FValueStatement -> E -> S -> IO (S, FValueStatement)
runVSLazyLet queueId (FValueStatementB assignments vs) env state = do
    (newState, newEnv) <- lazyRegisterAssignments queueId assignments state env
    seq newState $ runVS queueId vs newEnv newState

runVSInFoldF :: E -> IO (S, [FValueStatement]) -> FValueStatement -> IO (S, [FValueStatement])
runVSInFoldF env acc vs = do
    (s, vsl) <- acc
    (s, vs) <- makeRemoteArg env s vs
    return (s, vs:vsl)

makeRemoteArg :: E -> S -> FValueStatement -> IO (S, FValueStatement)
makeRemoteArg e s vs = do
    let (newLoc, newState) = getNewLoc s
    let newerState = putInLoc newLoc (False, e, FTypeT [], vs) newState
    return (newerState, FAValueStatement $ FFunApplicationR newLoc)

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
    (newerererState, nvs) <- runVS queueId vs newEnv newererState
    let newererererState = putInLoc vsLoc (False, newEnv, t, nvs) newerererState
    return (newererererState, newEnv)
forceRegisterAssignment queueId a@(FAssignmentB t pm vs) state env =
    setPM queueId t pm vs state env
forceRegisterAssignment queueId a _ _ = traceD (show a) undefined

lazyRegisterAssignment :: Int -> FAssignment -> S -> E -> IO (S, E)
lazyRegisterAssignment queueId (FAssignmentB t pm vs) state env =
    setPMLazy queueId t pm vs state env
lazyRegisterAssignment _ _ _ _ = undefined

setPM :: Int -> FType -> FPatternMatch -> FValueStatement -> S -> E -> IO (S, E)
setPM qId (FTypeT types) (FPatternMatchT pmL) vs state env = do
    (vss, newState) <- forceGetTupleVSS qId vs state env
    foldl (setPMInFoldF qId) (return (newState, env)) $ tList types pmL vss
setPM qId t (FPatternMatchB x) vs@(FAValueStatement _) state__ env_ = do
    let (loc, state_) = getNewLoc state__
    let env = registerLoc False env_ x loc
    (state_, vs) <- runVS qId vs env state_
    let state = putInLoc loc (False, env, t, vs) state_
    return (state, env)
setPM qId t (FPatternMatchB x) vs state env = do
    let (loc, newState) = getNewLoc state
    let newEnv = registerLoc False env x loc
    (newerState, nvs) <- runVS qId vs newEnv newState
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

forceGetTupleVSS :: Int -> FValueStatement -> S -> E -> IO ([FValueStatement], S)
forceGetTupleVSS queueId a@(FTValueStatement vss) s e = do
    (ns, tvs) <- runVS queueId a e s
    return (unwrapSingleTuples tvs, ns)
forceGetTupleVSS queueId a@(FAValueStatement funApplication) state env = do
    (vs, s) <- forceRunFunApplication queueId funApplication state env
    forceGetTupleVSS queueId vs s env
forceGetTupleVSS qId vs@(FSuspendedValue queueId) s e =
    return ([vs], s)
forceGetTupleVSS _ vs _ _ = traceD vs undefined

forceRunFunApplication :: Int -> FFunApplication -> S -> E -> IO (FValueStatement, S)
forceRunFunApplication queueId (FFunApplicationB "print" [str]) state env = do
    (s, r) <- runVS queueId str env state
    case r of
        FSuspendedValue qId -> do
            (ns, nr) <- runVS qId r env s
            print nr
            return (FTValueStatement [], ns)
        _ -> do
            print r
            return (FTValueStatement [], s)
forceRunFunApplication queueId (FFunApplicationB "set" [ref, value]) state env = do
    (newState, FRefAddr refAddr) <- runVS queueId ref env state
    let newerState = putInLoc refAddr (False, env, FTypeT [], value) newState
    return (FTValueStatement [], newerState)
forceRunFunApplication queueId (FFunApplicationB "get" [ref]) state env = do
    (newState, FRefAddr refAddr) <- runVS queueId ref env state
    let (_, e, _, refVS) = stateLookup refAddr newState
    (newerState, vs) <- runVS queueId refVS e newState
    return (vs, newerState)
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
            return (FTValueStatement [], newerState)
forceRunFunApplication queueId (FFunApplicationB "yield" []) state env =
    tryYield queueId state env
forceRunFunApplication queueId (FFunApplicationB "make_semaphore" []) state env = do
    let (SemaphoreT [] 0 semId, newState) = getNewSemaphore state
    return (FSemaphore semId, newState)
forceRunFunApplication queueId (FFunApplicationB "v" [semref]) state env = do
    (newState, FSemaphore semId) <- runVS queueId semref env state
    let (SemaphoreT blockedQueues semValue _) = getSemaphore semId newState
    if null blockedQueues
        then do
            let newerState = putSemaphore semId (SemaphoreT [] (semValue + 1) semId) newState
            return (FTValueStatement [], newerState)
        else do
            let newerState = putSemaphore semId (SemaphoreT (cutLast blockedQueues) semValue semId) newState
            return (FTValueStatement [], newerState)
forceRunFunApplication queueId a@(FFunApplicationB name args) state env = do
    (s, vs) <- runVS queueId (FAValueStatement a) env state
    return (vs, s)
forceRunFunApplication queueId a@(FFunApplicationR loc) state env = do
    (s, vs) <- runLoc queueId loc state
    return (vs, s)

forceRunFunApplication queueId a _ _ = traceD ("forceRunFunApplication " ++ show a) undefined

runUntilSemReady :: Int -> Int -> S -> E -> IO (FValueStatement, S)
runUntilSemReady queueId semId state env = do
    printD $ "runUnitlSemReady " ++ show queueId
    if anyAvailibleQueue state
        then do
            let availibleQueue = getAvailibleQueue state
            newState <- runQueue availibleQueue state
            let queue = getQueue queueId newState
            if checkNotBlocked queue newState
                then return (FTValueStatement [], newState)
                else runUntilSemReady queueId semId state env
        else fail "Deadlock"

tryYield :: Int -> S -> E -> IO (FValueStatement, S)
tryYield queueId state env = do
    printD $ "tryYield queueId = " ++ show queueId
    let newState = yieldQueue queueId state
    if anyAvailibleQueue newState
        then do
            let availibleQueue = getAvailibleQueue newState
            newerState <- runQueue availibleQueue newState
            let newestState = unyieldQueue queueId newerState
            return (FTValueStatement [], newestState)
        else do
            printD "nothing to yield to"
            return (FTValueStatement [], state)
        
oneStepEvaluation :: Int -> S -> E -> FValueStatement -> IO (S, E, FValueStatement)
oneStepEvaluation queueId s e (FAValueStatement (FFunApplicationR loc)) = oneStepRunLoc queueId loc s
oneStepEvaluation queueId s e (FAValueStatement (FFunApplicationB name vss)) = do
    let locs = lookupLoc name e
    oneStepRunLocs queueId locs vss e s
oneStepEvaluation queueId s e (FForceValueStatement assignments vs) = do
    (ns, ne) <- forceRegisterAssignments queueId assignments s e
    return (ns, ne, vs)
oneStepEvaluation queueId s e vs = traceD ("oneStepEvaluation " ++ show vs) undefined

oneStepRunLocs :: Int -> [Int] -> [FValueStatement] -> E -> S -> IO (S, E, FValueStatement)
oneStepRunLocs queueId (x:xs) args env state = do
    let funArgNames = funArgNamesLookup state x
    fits <- fitPatternMatchs queueId state env funArgNames args
    if fits
        then do
            let (iff, e, t, vs) = stateLookup x state
            (e, state) <- registerArgs queueId e env state funArgNames args
            let nstate = putInLoc x (iff, e, t, vs) state
            oneStepRunLoc queueId x nstate
        else oneStepRunLocs queueId xs args env state

oneStepRunLoc :: Int -> Int -> S -> IO (S, E, FValueStatement)
oneStepRunLoc queueId x state = do
    let (_, env, _, vs) = stateLookup x state
    return (state, env, vs)

registerArgs :: Int -> E -> E -> S -> [FPatternMatch] -> [FValueStatement] -> IO (E, S)
registerArgs queueId innerEnv argEnv state argNames vss = 
    foldl (registerArgsInFoldF queueId argEnv) (return (innerEnv, state)) $ dList argNames vss

registerArgsInFoldF :: Int -> E -> IO (E, S) -> (FPatternMatch, FValueStatement) -> IO (E, S)
registerArgsInFoldF queueId argEnv acc (FPatternMatchB str, vs) = do
    (e, s) <- acc
    let (newLoc, newState) = getNewLoc s
    let newEnv = registerLoc False e str newLoc
    let newerState = putInLoc newLoc (False, argEnv, FTypeT [], vs) newState
    return (newEnv, newerState)
registerArgsInFoldF queueId _ acc (FPatternMatchI _, _) = acc
registerArgsInFoldF queueId argEnv acc (FPatternMatchC _ pms, FCValueStatement _ vss) = do
    (e, s) <- acc
    registerArgs queueId e argEnv s pms vss
registerArgsInFoldF queueId argEnv acc a@(FPatternMatchT t1, FTValueStatement t2) = do
    (e, s) <- acc
    registerArgs queueId e argEnv s t1 t2
registerArgsInFoldF _ _ _ el = traceD ("registerArgsInFoldF " ++ show el) undefined

appendFAVS :: Int -> [Int] -> [FValueStatement] -> S -> IO (E, FValueStatement, Bool, [FPatternMatch], S)
appendFAVS _ [] vss state = fail "Non exhaustive pattern matches"
appendFAVS queueId (loc:xs) addVss state = do
    let argNames = funArgNamesLookup state loc
    let (_, e, _, vs) = stateLookup loc state
    fits <- fitPatternMatchs queueId state e argNames addVss
    if fits
        then do
            let (appendedVS, b, pms) = appendFAVSInt vs addVss
            return (e, appendedVS, b, pms, state)
        else 
            appendFAVS queueId xs addVss state

appendFAVSInt :: FValueStatement -> [FValueStatement] -> (FValueStatement, Bool, [FPatternMatch])
appendFAVSInt (FAValueStatement (FFunApplicationB funName vss)) addVss = (FAValueStatement $ FFunApplicationB funName (vss ++ addVss), False, [])
appendFAVSInt (FFValueStatement name vs) addVSS = (vs, True, [FPatternMatchB name])
appendFAVSInt x y = traceD ("appendFAVSInt " ++ show x ++ "\n" ++ show y) undefined

fitPatternMatchs :: Int -> S -> E -> [FPatternMatch] -> [FValueStatement] -> IO Bool
fitPatternMatchs _ s e _ [] = return True
fitPatternMatchs _ s e [] _ = return True
fitPatternMatchs queueId s e (pm:pms) (vs:vss) = do
    b <- fitPatternMatch queueId s e (pm, vs)
    if b 
        then fitPatternMatchs queueId s e pms vss
        else return False

fitPatternMatch :: Int -> S -> E -> (FPatternMatch, FValueStatement) -> IO Bool
fitPatternMatch _ s e (FPatternMatchI i1, vs@(FIValueStatement i2)) = return $ i1 == i2
fitPatternMatch queueId s e a@(FPatternMatchC (FPatternMatchB name1) pms, FCValueStatement name2 vss) = do
    res1 <- fitPatternMatchs queueId s e pms vss
    return $ name1 == name2 && res1
fitPatternMatch queueId s e (FPatternMatchB _, vs) = return True
fitPatternMatch queueId s e (pm@FPatternMatchC{}, vs@FAValueStatement{}) = do
    (ns, ne, nvs) <- oneStepEvaluation queueId s e vs
    fitPatternMatch queueId ns ne (pm, nvs)
fitPatternMatch queueId s e a@(FPatternMatchT tuple, vs@FAValueStatement{}) = do
    (ns, ne, nvs) <- oneStepEvaluation queueId s e vs
    fitPatternMatch queueId ns ne (FPatternMatchT tuple, nvs)
fitPatternMatch queueId s e a@(FPatternMatchT tuple, vs@FForceValueStatement{}) = do
    (ns, ne, nvs) <- oneStepEvaluation queueId s e vs
    fitPatternMatch queueId ns ne (FPatternMatchT tuple, nvs)
fitPatternMatch queueId s e (FPatternMatchT tuple, FTValueStatement tuple_) =
    fitPatternMatchs queueId s e tuple tuple_
fitPatternMatch queueId s e (pm , FForceValueStatement assignments vs) = do
    (ns, ne) <- forceRegisterAssignments queueId assignments s e
    fitPatternMatch queueId ns ne (pm, vs)
fitPatternMatch queueId s e (pm, FSusValueStatement vs) = fitPatternMatch queueId s e (pm, vs)
fitPatternMatch _ _ _ a = traceD ("fitPatternMatch undefined " ++ show a) undefined

wrapFunctionB :: Int -> FValueStatement -> E -> S -> IO (S, FValueStatement)
wrapFunctionB queueId vs@(FAValueStatement (FFunApplicationB x vss)) env state = do
    let locs = lookupLoc x env
    wrapFunctionBInt queueId locs vs env state

wrapFunctionBInt :: Int -> [Int] -> FValueStatement -> E -> S -> IO (S, FValueStatement)
wrapFunctionBInt queueId (loc:locs) vs@(FAValueStatement (FFunApplicationB x vss)) env state = do
    let funArgNames = funArgNamesLookup state loc
    fits <- fitPatternMatchs queueId state env funArgNames vss
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