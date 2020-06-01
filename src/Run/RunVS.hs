module Run.RunVS where

import StaticCheck.Format
import Util.Util
import Util.Env
import Util.State
import Control.Monad

runQueue :: QueueT -> S -> IO S
runQueue (QueueT env vs queueId _ _) state = do
    printD 1 $ "running queue with id " ++ show queueId
    (ns, nvs) <- runVS queueId vs env state
    return $ putInQueue queueId (QueueT env nvs queueId True False) ns

runSpecialFunction :: Int -> FValueStatement -> E -> S -> IO (S, FValueStatement)
runSpecialFunction queueId (FAValueStatement _ ap@(FFunApplicationB _ x r)) e s = do
    (nvs, ns) <- forceRunFunApplication queueId ap s e
    return (ns, nvs)
runSpecialFunction _ _ _ _ = undefined

checkSpecialFunctionName :: String -> Bool
checkSpecialFunctionName funName =
    funName `elem` ["print", "get", "set", "v", "p", "make_semaphore", "yield", "getline", "gets"]

isLambda FFValueStatement{} = True
isLambda _ = False

runLambda :: Int -> FValueStatement -> [FValueStatement] -> E -> E -> S -> IO (S, FValueStatement)
runLambda queueId (FFValueStatement _ argName vs) (argVs:argVss) innerEnv outerEnv state = do
    let (newLoc, newState) = getNewLoc state
    let ninnerEnv = registerLoc False innerEnv argName newLoc
    let newerState = putInLoc newLoc (outerEnv, argVs) newState
    runLambda queueId vs argVss ninnerEnv outerEnv newerState
runLambda queueId vs [] innerEnv outerEnv state =
    runVS queueId vs innerEnv state
runLambda queueId vs args innerEnv outerEnv state = do
    (newState, nvs) <- runVS queueId vs innerEnv state
    if isLambda nvs
        then runLambda queueId nvs args innerEnv outerEnv newState
        else undefined -- infinite loop

runLambdaWrapper :: Int -> FValueStatement -> E -> [FPatternMatch] -> [FValueStatement] -> E -> S -> IO (S, FValueStatement)
runLambdaWrapper queueId vs innerEnv argPMs vss outerEnv state = do
    let argVSs = take (length argPMs) vss
    let restVSs = takeLast (length vss - length argPMs) vss
    (innerEnv, newState) <- registerArgs queueId innerEnv outerEnv state argPMs argVSs
    runLambda queueId vs restVSs innerEnv outerEnv newState
        
getMatchingVSForName :: Int -> [Int] -> [FValueStatement] -> E -> S -> IO (FValueStatement, E, Int, S, [FPatternMatch], [FValueStatement])
getMatchingVSForName _ [] _ _ _ = fail "not exhaustive pattern matching"
getMatchingVSForName queueId (loc:locs) argVss outerEnv state = do
    let funArgNames = funArgNamesLookup state loc
    (fits, state, argVss) <- fitPatternMatchs queueId state outerEnv funArgNames argVss
    if fits
        then do
            let (innerEnv, vs) = stateLookup loc state
            return (vs, innerEnv, loc, state, funArgNames, argVss)
        else getMatchingVSForName queueId locs argVss outerEnv state

runNotSpecialFunction :: Int -> FValueStatement -> E -> S -> IO (S, FValueStatement)
runNotSpecialFunction queueId a@(FAValueStatement _ (FFunApplicationB _ funName funArgVss)) outerEnv state = do
    let locs = lookupLoc funName outerEnv
    (vs, innerEnv, loc, state, funArgNames, funArgVss) <- getMatchingVSForName queueId locs funArgVss outerEnv state
    if isLambda vs 
        then
            runLambdaWrapper queueId vs innerEnv funArgNames funArgVss outerEnv state
        else
            if length funArgVss < length funArgNames
                then
                    wrapFunctionB queueId a innerEnv state
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
                            (innerEnv, state) <- registerArgs queueId innerEnv outerEnv state funArgNames funArgVss
                            runVS queueId vs innerEnv state
runNotSpecialFunction _ _ _ _ = undefined

runVS :: Int -> FValueStatement -> E -> S -> IO (S, FValueStatement)
runVS queueId vs@FForceValueStatement{} e s = runVSForceLet queueId vs e s
runVS queueId vs@FIValueStatement{} e s = return (s, vs)
runVS queueId vs@(FAValueStatement _ FFunApplicationB{}) e s = runVSFunB queueId vs e s
runVS queueId vs@(FExpr _ FEMul{}) e s = runVSMul queueId vs e s
runVS queueId vs@FIfValueStatement{} e s = runVSIf queueId vs e s
runVS queueId vs@(FExpr _ FEEQ{}) e s = runVSEQ queueId vs e s
runVS queueId vs@(FExpr _ FESub{}) e s = runVSSub queueId vs e s
runVS queueId vs@(FExpr _ FEAdd{}) e s = runVSAdd queueId vs e s
runVS queueId vs@(FAValueStatement _ (FFunApplicationR loc)) e s = runVSFunApplR queueId vs e s
runVS queueId vs@FCValueStatement{} e s = runVSC queueId vs e s
runVS queueId vs@FTValueStatement{} e s = runVST queueId vs e s
runVS queueId vs@FRefAddr{} e s = return (s, vs)
runVS queueId vs@FFValueStatement{} e s = runVSLam queueId vs e s
runVS queueId vs@FSusValueStatement{} e s = runVSSusSt queueId vs e s
runVS queueId vs@FSuspendedValue{} e s = runVSSusVal queueId vs e s
runVS queueId vs@FSemaphore{} e s = return (s, vs)
runVS queueId vs@FValueStatementB{} e s = runVSLazyLet queueId vs e s
runVS queueId vs@(FNTValueStatement n (FTValueStatement _ ts)) e s = runVS queueId (takeNth n ts) e s
runVS queueId vs@(FNTValueStatement n (FCValueStatement _ _ ts)) e s = runVS queueId (takeNth n ts) e s
runVS queueId vs@FNTValueStatement{} e s = runVSFNTNotTuple queueId vs e s
runVS queueId vs@(FExpr _ FEMod{}) e s = runVSMod queueId vs e s
runVS queueId vs@(FExpr _ FEDiv{}) e s = runVSDiv queueId vs e s
runVS queueId vs@(FExpr _ FEL{}) e s = runVSL queueId vs e s
runVS queueId vs@(FExpr _ FELQ{}) e s = runVSLQ queueId vs e s
runVS queueId vs@(FExpr _ FEG{}) e s = runVSG queueId vs e s
runVS queueId vs@(FExpr _ FEGQ{}) e s = runVSGQ queueId vs e s
runVS queueId vs@(FExpr _ FENE{}) e s = runVSNE queueId vs e s

runVSL :: Int -> FValueStatement -> E -> S -> IO (S, FValueStatement)
runVSL queueId (FExpr _ (FEL vs1 vs2)) e s = do
    (s, FIValueStatement _ i1) <- runVS queueId vs1 e s
    (s, FIValueStatement _ i2) <- runVS queueId vs2 e s
    return (s, FIValueStatement Nothing $ if i1 < i2 then 1 else 0)
runVSL _ _ _ _ = undefined

runVSLQ :: Int -> FValueStatement -> E -> S -> IO (S, FValueStatement)
runVSLQ queueId (FExpr _ (FELQ vs1 vs2)) e s = do
    (s, FIValueStatement _ i1) <- runVS queueId vs1 e s
    (s, FIValueStatement _ i2) <- runVS queueId vs2 e s
    return (s, FIValueStatement Nothing $ if i1 <= i2 then 1 else 0)
runVSLQ _ _ _ _ = undefined

runVSG :: Int -> FValueStatement -> E -> S -> IO (S, FValueStatement)
runVSG queueId (FExpr _ (FEG vs1 vs2)) e s = do
    (s, FIValueStatement _ i1) <- runVS queueId vs1 e s
    (s, FIValueStatement _ i2) <- runVS queueId vs2 e s
    return (s, FIValueStatement Nothing $ if i1 > i2 then 1 else 0)
runVSG _ _ _ _ = undefined

runVSGQ :: Int -> FValueStatement -> E -> S -> IO (S, FValueStatement)
runVSGQ queueId (FExpr _ (FEGQ vs1 vs2)) e s = do
    (s, FIValueStatement _ i1) <- runVS queueId vs1 e s
    (s, FIValueStatement _ i2) <- runVS queueId vs2 e s
    return (s, FIValueStatement Nothing $ if i1 >= i2 then 1 else 0)
runVSGQ _ _ _ _ = undefined

runVSNE :: Int -> FValueStatement -> E -> S -> IO (S, FValueStatement)
runVSNE queueId (FExpr _ (FENE vs1 vs2)) e s = do
    (s, FIValueStatement _ i1) <- runVS queueId vs1 e s
    (s, FIValueStatement _ i2) <- runVS queueId vs2 e s
    return (s, FIValueStatement Nothing $ if i1 /= i2 then 1 else 0)
runVSNE _ _ _ _ = undefined

runVSMod :: Int -> FValueStatement -> E -> S -> IO (S, FValueStatement)
runVSMod queueId (FExpr _ (FEMod vs1 vs2)) e s = do
    (s, FIValueStatement _ i2) <- runVS queueId vs2 e s
    if i2 == 0
        then fail "division by zero"
        else do
            (s, FIValueStatement _ i1) <- runVS queueId vs1 e s
            return (s, FIValueStatement Nothing $ mod i1 i2)
runVSMod _ _ _ _ = undefined

runVSDiv :: Int -> FValueStatement -> E -> S -> IO (S, FValueStatement)
runVSDiv queueId (FExpr _ (FEDiv vs1 vs2)) e s = do
    (s, FIValueStatement _ i2) <- runVS queueId vs2 e s
    if i2 == 0
        then fail "division by zero"
        else do
            (s, FIValueStatement _ i1) <- runVS queueId vs1 e s
            return (s, FIValueStatement Nothing $ div i1 i2)
runVSDiv _ _ _ _ = undefined

runVSFunApplR :: Int -> FValueStatement -> E -> S -> IO (S, FValueStatement)
runVSFunApplR queueId (FAValueStatement Nothing (FFunApplicationR loc)) e s = do
    let (innerEnv, vs) = stateLookup loc s
    (s, vs) <- runVS queueId vs innerEnv s
    let s2 = putInLoc loc (innerEnv, vs) s
    return (s2, vs)
runVSFunApplR _ _ _ _ = undefined

runVSFNTNotTuple :: Int -> FValueStatement -> E -> S -> IO (S, FValueStatement)
runVSFNTNotTuple queueId (FNTValueStatement n vs) e s = do
    a <- runVS queueId vs e s
    case a of
        (s, FTValueStatement _ ts) -> return (s, takeNth n ts)
        (s, FCValueStatement _ _ ts) -> return (s, takeNth n ts)
        _ -> undefined
runVSFNTNotTuple _ _ _ _ = undefined

runVSMul :: Int -> FValueStatement -> E -> S -> IO (S, FValueStatement) 
runVSMul queueId (FExpr _ (FEMul vs1 vs2)) env state = do
    (newState, FIValueStatement _ i1) <- runVS queueId vs1 env state
    (newerState, FIValueStatement _ i2) <- runVS queueId vs2 env newState 
    return (newerState, FIValueStatement Nothing $ i1 * i2)
runVSMul _ _ _ _ = undefined

runVSEQ :: Int -> FValueStatement -> E -> S -> IO (S, FValueStatement)
runVSEQ queueId vs@(FExpr _ (FEEQ vs1 vs2)) env state = do
    (newState, FIValueStatement _ i1) <- runVS queueId vs1 env state
    (newerState, FIValueStatement _ i2) <- runVS queueId vs2 env newState
    return (newerState, FIValueStatement Nothing $ if i1 == i2 then 1 else 0)
runVSEQ _ _ _ _ = undefined

runVSSub :: Int -> FValueStatement -> E -> S -> IO (S, FValueStatement)
runVSSub queueId (FExpr _ (FESub vs1 vs2)) env state = do
    (newState, FIValueStatement _ i1) <- runVS queueId vs1 env state
    (newerState, FIValueStatement _ i2) <- runVS queueId vs2 env newState
    return (newerState, FIValueStatement Nothing (i1 - i2))
runVSSub _ _ _ _ = undefined

runVSForceLet :: Int -> FValueStatement -> E -> S -> IO (S, FValueStatement)
runVSForceLet queueId (FForceValueStatement _ assignments vs) env state = do
    (newState, newEnv) <- forceRegisterAssignments queueId assignments state env
    seq newState $ runVS queueId vs newEnv newState
runVSForceLet _ _ _ _ = undefined

runVSIf :: Int -> FValueStatement -> E -> S -> IO (S, FValueStatement)
runVSIf queueId (FIfValueStatement _ condvs res1vs res2vs) env state = do
    (state, FIValueStatement _ condVal) <- runVS queueId condvs env state
    if condVal /= 0
        then runVS queueId res1vs env state
        else runVS queueId res2vs env state
runVSIf _ _ _ _ = undefined

runVSFunB :: Int -> FValueStatement -> E -> S -> IO (S, FValueStatement)
runVSFunB queueId a@(FAValueStatement _ (FFunApplicationB _ funName _)) =
    if checkSpecialFunctionName funName
        then
            runSpecialFunction queueId a
        else 
            runNotSpecialFunction queueId a
runVSFunB _ _ = undefined

runVSAdd :: Int -> FValueStatement -> E -> S -> IO (S, FValueStatement)
runVSAdd queueId vs@(FExpr _ (FEAdd vs1 vs2)) env state = do
    (newState, FIValueStatement _ i1) <- runVS queueId vs1 env state
    (newerState, FIValueStatement _ i2) <- runVS queueId vs2 env newState
    return (newerState, FIValueStatement Nothing (i1 + i2))
runVSAdd _ _ _ _ = undefined
    
runLoc :: Int -> Int -> S -> IO (S, FValueStatement)
runLoc queueId x state = do
    let (innerEnv, vs) = stateLookup x state
    runVS queueId vs innerEnv state

runVSC :: Int -> FValueStatement -> E -> S -> IO (S, FValueStatement)
runVSC queueId (FCValueStatement _ name vss) e s = do
    (new_state, newVss) <- foldl (cvsInFoldF queueId e) (return (s, [])) vss
    return (new_state, FCValueStatement Nothing name (reverse newVss))
runVSC _ _ _ _ = undefined

cvsInFoldF :: Int -> E -> IO (S, [FValueStatement]) -> FValueStatement -> IO (S, [FValueStatement])
cvsInFoldF queueId e acc vs = do
    (state, vsl) <- acc
    (new_state, vs_) <- runVS queueId vs e state
    return (new_state, vs_:vsl)

runVST :: Int -> FValueStatement -> E -> S -> IO (S, FValueStatement)
runVST queueId (FTValueStatement _ xss) e s = do
    (ns, xssc) <- foldl (runTVSInFoldF queueId e) (return (s, [])) xss
    return (ns, FTValueStatement Nothing xssc)
runVST _ _ _ _ = undefined

runTVSInFoldF :: Int -> E -> IO (S, [FValueStatement]) -> FValueStatement -> IO (S, [FValueStatement])
runTVSInFoldF queueId env acc vs = do
    (s, vsl) <- acc
    (ns, nvs) <- runVS queueId vs env s
    return (ns, nvs:vsl)

runVSLam :: Int -> FValueStatement -> E -> S -> IO (S, FValueStatement)
runVSLam queueId a@(FFValueStatement _ argName vs) e s =
    return (s, a)
runVSLam _ _ _ _ = undefined

runVSSusSt :: Int -> FValueStatement -> E -> S -> IO (S, FValueStatement)
runVSSusSt queueId (FSusValueStatement vs) e s = do
    let queueId = getFreeQueueId s
    let nvs = FSuspendedValue queueId
    let ns = putQueue s (QueueT e vs queueId False False)
    return (ns, nvs)
runVSSusSt _ _ _ _ = undefined

runVSSusVal :: Int -> FValueStatement -> E -> S -> IO (S, FValueStatement)
runVSSusVal queueId (FSuspendedValue qId) e s_ = do
    let s = yieldQueue queueId s_
    let (QueueT env qvs _ b y) = getQueue qId s
    (ns, nvs) <- runVS qId qvs env s
    let nns = putInQueue qId (QueueT env qvs qId True False) ns
    return (unyieldQueue queueId nns, nvs)
runVSSusVal _ _ _ _ = undefined

runVSLazyLet :: Int -> FValueStatement -> E -> S -> IO (S, FValueStatement)
runVSLazyLet queueId (FValueStatementB _ assignments vs) env state = do
    (newState, newEnv) <- lazyRegisterAssignments queueId assignments state env
    seq newState $ runVS queueId vs newEnv newState
runVSLazyLet _ _ _ _ = undefined

runVSInFoldF :: E -> IO (S, [FValueStatement]) -> FValueStatement -> IO (S, [FValueStatement])
runVSInFoldF env acc vs = do
    (s, vsl) <- acc
    (s, vs) <- makeRemoteArg env s vs
    return (s, vs:vsl)

makeRemoteArg :: E -> S -> FValueStatement -> IO (S, FValueStatement)
makeRemoteArg e s vs = do
    let (newLoc, newState) = getNewLoc s
    let newerState = putInLoc newLoc (e, vs) newState
    return (newerState, FAValueStatement Nothing $ FFunApplicationR newLoc)

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
forceRegisterAssignment queueId (FAssignmentB _ typ@(FTypeB _ "Ref" [t]) (FPatternMatchB _ name) vs) state env = do
    let (refLoc, newState) = getNewLoc state
    let (vsLoc, newerState) = getNewLoc newState
    let refVS = FRefAddr vsLoc
    let newEnv = registerLoc False env name refLoc
    let newererState = putInLoc refLoc (newEnv, refVS) newerState
    (newerererState, nvs) <- runVS queueId vs newEnv newererState
    let newererererState = putInLoc vsLoc (newEnv, nvs) newerererState
    return (newererererState, newEnv)
forceRegisterAssignment queueId (FAssignmentB _ t pm vs) state env =
    setPM queueId pm vs state env
forceRegisterAssignment queueId (FRefAssignment _ (FRefDef _ _ name vs)) state env = do
    let (refLoc, newState) = getNewLoc state
    let (vsLoc, newerState) = getNewLoc newState
    let refVS = FRefAddr vsLoc
    let newEnv = registerLoc False env name refLoc
    let newererState = putInLoc refLoc (newEnv, refVS) newerState
    (newerererState, nvs) <- runVS queueId vs newEnv newererState
    let newererererState = putInLoc vsLoc (newEnv, nvs) newerererState
    return (newererererState, newEnv)

lazyRegisterAssignment :: Int -> FAssignment -> S -> E -> IO (S, E)
lazyRegisterAssignment queueId (FAssignmentB _ (FTypeB _ "Ref" [_]) (FPatternMatchB _ name) vs) state env = do
    let (refLoc, newState) = getNewLoc state
    let (vsLoc, newerState) = getNewLoc newState
    let refVS = FRefAddr vsLoc
    let newEnv = registerLoc False env name refLoc
    let newererState = putInLoc refLoc (newEnv, refVS) newerState
    let newerererState = putInLoc vsLoc (newEnv, vs) newererState
    return (newerererState, newEnv)
lazyRegisterAssignment queueId (FAssignmentB _ _ pm vs) state env =
    setPMLazy queueId pm vs state env
lazyRegisterAssignment queueId (FRefAssignment _ (FRefDef _ _ name vs)) state env = do
    let (refLoc, newState) = getNewLoc state
    let (vsLoc, newerState) = getNewLoc newState
    let refVS = FRefAddr vsLoc
    let newEnv = registerLoc False env name refLoc
    let newererState = putInLoc refLoc (newEnv, refVS) newerState
    let newerererState = putInLoc vsLoc (newEnv, vs) newererState
    return (newerererState, newEnv)

setPM :: Int -> FPatternMatch -> FValueStatement -> S -> E -> IO (S, E)
setPM qId (FPatternMatchT _ pmL) vs state env = do
    (vss, newState) <- forceGetTupleVSS qId vs state env
    foldl (setPMInFoldF qId) (return (newState, env)) $ dList pmL vss
setPM qId (FPatternMatchB _ x) vs state env = do
    let (loc, newState) = getNewLoc state
    let newEnv = registerLoc False env x loc
    (newerState, nvs) <- runVS qId vs newEnv newState
    let newererState = putInLoc loc (newEnv, nvs) newerState
    return (newererState, newEnv)
setPM qId FPatternMatchI{} vs state env = do
    (state, _) <- runVS qId vs env state
    return (state, env)
setPM qId pm@FPatternMatchC{} vs state env = do
    (state, vs) <- runVS qId vs env state
    (env, state) <- registerArgs qId env env state [pm] [vs]
    return (state, env)

setPMLazy :: Int -> FPatternMatch -> FValueStatement -> S -> E -> IO (S, E)
setPMLazy queueId pm vs state env = do
    (e, s) <- registerArgs queueId env env state [pm] [vs]
    return (s, e)

setPMInFoldF :: Int -> IO (S, E) -> (FPatternMatch, FValueStatement) -> IO (S, E)
setPMInFoldF queueId acc (pm, vs) = do
    (s, e) <- acc
    setPM queueId pm vs s e

unwrapSingleTuples :: FValueStatement -> [FValueStatement]
unwrapSingleTuples (FTValueStatement _ [vs]) = unwrapSingleTuples vs
unwrapSingleTuples (FTValueStatement _ vss) = vss
unwrapSingleTuples a = [a]

isSuspendedValue :: FValueStatement -> Bool
isSuspendedValue FSuspendedValue{} = True
isSuspendedValue _ = False

forceGetTupleVSS :: Int -> FValueStatement -> S -> E -> IO ([FValueStatement], S)
forceGetTupleVSS queueId a@(FTValueStatement _ vss) s e = do
    (ns, tvs) <- runVS queueId a e s
    return (unwrapSingleTuples tvs, ns)
forceGetTupleVSS queueId a@(FAValueStatement _ funApplication) state env = do
    (vs, s) <- forceRunFunApplication queueId funApplication state env
    if isSuspendedValue vs then return ([vs], s) else forceGetTupleVSS queueId vs s env
forceGetTupleVSS qId (FSuspendedValue queueId) s e = do
    let queue = getQueue queueId s
    s <- runQueue queue s
    let (QueueT _ vs _ _ _) = getQueue queueId s
    forceGetTupleVSS qId vs s e
forceGetTupleVSS queueId a@FValueStatementB{} s e = do
    (s, vs) <- runVS queueId a e s
    if isSuspendedValue vs then return ([vs], s) else forceGetTupleVSS queueId vs s e
forceGetTupleVSS queueId a@FForceValueStatement{} s e = do
    (s, vs) <- runVS queueId a e s
    if isSuspendedValue vs then return ([vs], s) else forceGetTupleVSS queueId vs s e
forceGetTupleVSS queueId a@FIfValueStatement{} s e = do
    (s, vs) <- runVS queueId a e s
    if isSuspendedValue vs then return ([vs], s) else forceGetTupleVSS queueId vs s e
forceGetTupleVSS _ vs@FIValueStatement{} s _ = return ([vs], s)
forceGetTupleVSS _ vs@FFValueStatement{} s _ = return ([vs], s)
forceGetTupleVSS queueId vs@FCValueStatement{} s e = do
    (s, vs) <- runVS queueId vs e s
    return ([vs], s)
forceGetTupleVSS queueId vs@FExpr{} s e = do
    (s, vs) <- runVS queueId vs e s
    return ([vs], s)
forceGetTupleVSS _ vs@FRefAddr{} s _ = return ([vs], s)
forceGetTupleVSS _ vs@FSemaphore{} s _ = return ([vs], s)
forceGetTupleVSS queueId vs@FSusValueStatement{} s e = do
    (s, vs) <- runVS queueId vs e s
    return ([vs], s)
forceGetTupleVSS queueId vs@FNTValueStatement{} s e = do
    (s, vs) <- runVS queueId vs e s
    if isSuspendedValue vs then return ([vs], s) else forceGetTupleVSS queueId vs s e

getNChars :: Int -> IO String
getNChars 0 = return ""
getNChars d = do
    c <- getChar
    s <- getNChars $ d - 1
    return (c:s)

forceRunFunApplication :: Int -> FFunApplication -> S -> E -> IO (FValueStatement, S)
forceRunFunApplication queueId (FFunApplicationB _ "print" [str]) state env = do
    (s, r) <- runVS queueId str env state
    case r of
        FSuspendedValue qId -> do
            (ns, nr) <- runVS queueId r env s
            print nr
            return (FTValueStatement Nothing [], ns)
        _ -> do
            print r
            return (FTValueStatement Nothing [], s)
forceRunFunApplication queueId (FFunApplicationB _ "getline" []) state env = do
    s <- getLine
    return (convertString Nothing s, state)
forceRunFunApplication queueId (FFunApplicationB _ "gets" [x]) state env = do
    (state, FIValueStatement _ d) <- runVS queueId x env state
    s <- getNChars d
    return (convertString Nothing s, state)
forceRunFunApplication queueId (FFunApplicationB _ "set" [ref, value]) state env = do
    (newState, FRefAddr refAddr) <- runVS queueId ref env state
    let newerState = putInLoc refAddr (env, value) newState
    return (FTValueStatement Nothing [], newerState)
forceRunFunApplication queueId (FFunApplicationB _ "get" [ref]) state env = do
    (newState, FRefAddr refAddr) <- runVS queueId ref env state
    let (e, refVS) = stateLookup refAddr newState
    (newerState, vs) <- runVS queueId refVS e newState
    return (vs, newerState)
forceRunFunApplication queueId (FFunApplicationB _ "p" [semref]) state env = do
    (newState, FSemaphore semId) <- runVS queueId semref env state
    let (SemaphoreT blockedQueues semValue _) = getSemaphore semId state
    if semValue <= 0
        then do
            let newerState = putSemaphore semId (SemaphoreT (queueId:blockedQueues) semValue semId) newState
            let availibleQueue = getAvailibleQueue newerState
            runUntilSemReady queueId semId newerState env
        else do
            let newerState = putSemaphore semId (SemaphoreT blockedQueues (semValue - 1) semId) newState
            return (FTValueStatement Nothing [], newerState)
forceRunFunApplication queueId (FFunApplicationB _ "yield" []) state env =
    tryYield queueId state env
forceRunFunApplication queueId (FFunApplicationB _ "make_semaphore" []) state env = do
    let (SemaphoreT [] 0 semId, newState) = getNewSemaphore state
    return (FSemaphore semId, newState)
forceRunFunApplication queueId (FFunApplicationB _ "v" [semref]) state env = do
    (newState, FSemaphore semId) <- runVS queueId semref env state
    let (SemaphoreT blockedQueues semValue _) = getSemaphore semId newState
    if null blockedQueues
        then do
            let newerState = putSemaphore semId (SemaphoreT [] (semValue + 1) semId) newState
            return (FTValueStatement Nothing [], newerState)
        else do
            let newerState = putSemaphore semId (SemaphoreT (cutLast blockedQueues) semValue semId) newState
            return (FTValueStatement Nothing [], newerState)
forceRunFunApplication queueId a@(FFunApplicationB _ name args) state env = do
    (s, vs) <- runVS queueId (FAValueStatement Nothing a) env state
    return (vs, s)
forceRunFunApplication queueId a@(FFunApplicationR loc) state env = do
    (s, vs) <- runLoc queueId loc state
    return (vs, s)


runUntilSemReady :: Int -> Int -> S -> E -> IO (FValueStatement, S)
runUntilSemReady queueId semId state env =
    if anyAvailibleQueue state
        then do
            let availibleQueue = getAvailibleQueue state
            newState <- runQueue availibleQueue state
            let queue = getQueue queueId newState
            if checkNotBlocked queue newState
                then return (FTValueStatement Nothing [], newState)
                else runUntilSemReady queueId semId state env
        else fail "Deadlock"

tryYield :: Int -> S -> E -> IO (FValueStatement, S)
tryYield queueId state env = do
    let newState = yieldQueue queueId state
    if anyAvailibleQueue newState
        then do
            let availibleQueue = getAvailibleQueue newState
            newerState <- runQueue availibleQueue newState
            let newestState = unyieldQueue queueId newerState
            return (FTValueStatement Nothing [], newestState)
        else return (FTValueStatement Nothing [], state)
        
oneStepEvaluation :: Int -> S -> E -> FValueStatement -> IO (S, E, FValueStatement)
oneStepEvaluation queueId s e (FAValueStatement _ (FFunApplicationR loc)) = oneStepRunLoc queueId loc s
oneStepEvaluation queueId s e (FAValueStatement _ (FFunApplicationB _ name vss)) = do
    let locs = lookupLoc name e
    oneStepRunLocs queueId locs vss e s
oneStepEvaluation queueId s e (FForceValueStatement _ assignments vs) = do
    (ns, ne) <- forceRegisterAssignments queueId assignments s e
    return (ns, ne, vs)
oneStepEvaluation queueId s e (FValueStatementB _ assignments vs) = do
    (ns, ne) <- lazyRegisterAssignments queueId assignments s e
    return (ns, ne, vs)
oneStepEvaluation queueId s e (FIfValueStatement _ condvs vs1 vs2) = do
    (s, FIValueStatement _ r) <- runVS queueId condvs e s
    if r == 0
        then return (s, e, vs2)
        else return (s, e, vs1)
oneStepEvaluation queueId s e (FTValueStatement _ ts) = do
    (s, e, ts) <- oneStepTupleEvaluation queueId s e ts
    return (s, e, FTValueStatement Nothing ts)
oneStepEvaluation queueId s e vs@FIValueStatement{} = return (s, e, vs)
oneStepEvaluation queueId s e vs@FFValueStatement{} = return (s, e, vs)
oneStepEvaluation queueId s e vs@FRefAddr{} = return (s, e, vs)
oneStepEvaluation queueId s e (FExpr _ (FEAdd (FIValueStatement _ i1) (FIValueStatement _ i2))) =
    return (s, e, FIValueStatement Nothing $ i1 + i2)
oneStepEvaluation queueId s e (FExpr _ (FEAdd i1@FIValueStatement{} vs2)) = do
    (s, e, vs2) <- oneStepEvaluation queueId s e vs2
    return (s, e, FExpr Nothing $ FEAdd i1 vs2)
oneStepEvaluation queueId s e (FExpr _ (FEAdd vs1 vs2)) = do
    (s, e, vs1) <- oneStepEvaluation queueId s e vs1
    return (s, e, FExpr Nothing $ FEAdd vs1 vs2)
oneStepEvaluation queueId s e (FExpr _ (FESub (FIValueStatement _ i1) (FIValueStatement _ i2))) =
    return (s, e, FIValueStatement Nothing $ i1 - i2)
oneStepEvaluation queueId s e (FExpr _ (FESub i1@FIValueStatement{} vs2)) = do
    (s, e, vs2) <- oneStepEvaluation queueId s e vs2
    return (s, e, FExpr Nothing $ FESub i1 vs2)
oneStepEvaluation queueId s e (FExpr _ (FESub vs1 vs2)) = do
    (s, e, vs1) <- oneStepEvaluation queueId s e vs1
    return (s, e, FExpr Nothing $ FESub vs1 vs2)
oneStepEvaluation queueId s e (FExpr _ (FEMul (FIValueStatement _ i1) (FIValueStatement _ i2))) =
    return (s, e, FIValueStatement Nothing $ i1 * i2)
oneStepEvaluation queueId s e (FExpr _ (FEMul i1@FIValueStatement{} vs2)) = do
    (s, e, vs2) <- oneStepEvaluation queueId s e vs2
    return (s, e, FExpr Nothing $ FEMul i1 vs2)
oneStepEvaluation queueId s e (FExpr _ (FEMul vs1 vs2)) = do
    (s, e, vs1) <- oneStepEvaluation queueId s e vs1
    return (s, e, FExpr Nothing $ FEMul vs1 vs2)
oneStepEvaluation queueId s e (FExpr _ (FEMod _ (FIValueStatement _ 0))) = fail "division by zero"
oneStepEvaluation queueId s e (FExpr _ (FEMod (FIValueStatement _ i1) (FIValueStatement _ i2))) =
    return (s, e, FIValueStatement Nothing $ mod i1 i2)
oneStepEvaluation queueId s e (FExpr _ (FEMod i1@FIValueStatement{} vs2)) = do
    (s, e, vs2) <- oneStepEvaluation queueId s e vs2
    return (s, e, FExpr Nothing $ FEMod i1 vs2)
oneStepEvaluation queueId s e (FExpr _ (FEMod vs1 vs2)) = do
    (s, e, vs1) <- oneStepEvaluation queueId s e vs1
    return (s, e, FExpr Nothing $ FEMod vs1 vs2)
oneStepEvaluation queueId s e (FExpr _ (FEDiv _ (FIValueStatement _ 0))) = fail "division by zero"
oneStepEvaluation queueId s e (FExpr _ (FEDiv (FIValueStatement _ i1) (FIValueStatement _ i2))) =
    return (s, e, FIValueStatement Nothing $ div i1 i2)
oneStepEvaluation queueId s e (FExpr _ (FEDiv i1@FIValueStatement{} vs2)) = do
    (s, e, vs2) <- oneStepEvaluation queueId s e vs2
    return (s, e, FExpr Nothing $ FEDiv i1 vs2)
oneStepEvaluation queueId s e (FExpr _ (FEDiv vs1 vs2)) = do
    (s, e, vs1) <- oneStepEvaluation queueId s e vs1
    return (s, e, FExpr Nothing $ FEDiv vs1 vs2)
oneStepEvaluation queueId s e (FExpr _ (FEL (FIValueStatement _ i1) (FIValueStatement _ i2))) =
    return (s, e, FIValueStatement Nothing $ if i1 < i2 then 1 else 0)
oneStepEvaluation queueId s e (FExpr _ (FEL i1@FIValueStatement{} vs2)) = do
    (s, e, vs2) <- oneStepEvaluation queueId s e vs2
    return (s, e, FExpr Nothing $ FEL i1 vs2)
oneStepEvaluation queueId s e (FExpr _ (FEL vs1 vs2)) = do
    (s, e, vs1) <- oneStepEvaluation queueId s e vs1
    return (s, e, FExpr Nothing $ FEL vs1 vs2)
oneStepEvaluation queueId s e (FExpr _ (FELQ (FIValueStatement _ i1) (FIValueStatement _ i2))) =
    return (s, e, FIValueStatement Nothing $ if i1 <= i2 then 1 else 0)
oneStepEvaluation queueId s e (FExpr _ (FELQ i1@FIValueStatement{} vs2)) = do
    (s, e, vs2) <- oneStepEvaluation queueId s e vs2
    return (s, e, FExpr Nothing $ FELQ i1 vs2)
oneStepEvaluation queueId s e (FExpr _ (FELQ vs1 vs2)) = do
    (s, e, vs1) <- oneStepEvaluation queueId s e vs1
    return (s, e, FExpr Nothing $ FELQ vs1 vs2)
oneStepEvaluation queueId s e (FExpr _ (FEG (FIValueStatement _ i1) (FIValueStatement _ i2))) =
    return (s, e, FIValueStatement Nothing $ if i1 > i2 then 1 else 0)
oneStepEvaluation queueId s e (FExpr _ (FEG i1@FIValueStatement{} vs2)) = do
    (s, e, vs2) <- oneStepEvaluation queueId s e vs2
    return (s, e, FExpr Nothing $ FEG i1 vs2)
oneStepEvaluation queueId s e (FExpr _ (FEG vs1 vs2)) = do
    (s, e, vs1) <- oneStepEvaluation queueId s e vs1
    return (s, e, FExpr Nothing $ FEG vs1 vs2)
oneStepEvaluation queueId s e (FExpr _ (FEGQ (FIValueStatement _ i1) (FIValueStatement _ i2))) =
    return (s, e, FIValueStatement Nothing $ if i1 >= i2 then 1 else 0)
oneStepEvaluation queueId s e (FExpr _ (FEGQ i1@FIValueStatement{} vs2)) = do
    (s, e, vs2) <- oneStepEvaluation queueId s e vs2
    return (s, e, FExpr Nothing $ FEGQ i1 vs2)
oneStepEvaluation queueId s e (FExpr _ (FEGQ vs1 vs2)) = do
    (s, e, vs1) <- oneStepEvaluation queueId s e vs1
    return (s, e, FExpr Nothing $ FEGQ vs1 vs2)
oneStepEvaluation queueId s e (FExpr _ (FEEQ (FIValueStatement _ i1) (FIValueStatement _ i2))) =
    return (s, e, FIValueStatement Nothing $ if i1 == i2 then 1 else 0)
oneStepEvaluation queueId s e (FExpr _ (FEEQ i1@FIValueStatement{} vs2)) = do
    (s, e, vs2) <- oneStepEvaluation queueId s e vs2
    return (s, e, FExpr Nothing $ FEEQ i1 vs2)
oneStepEvaluation queueId s e (FExpr _ (FEEQ vs1 vs2)) = do
    (s, e, vs1) <- oneStepEvaluation queueId s e vs1
    return (s, e, FExpr Nothing $ FEEQ vs1 vs2)
oneStepEvaluation queueId s e (FExpr _ (FENE (FIValueStatement _ i1) (FIValueStatement _ i2))) =
    return (s, e, FIValueStatement Nothing $ if i1 /= i2 then 1 else 0)
oneStepEvaluation queueId s e (FExpr _ (FENE i1@FIValueStatement{} vs2)) = do
    (s, e, vs2) <- oneStepEvaluation queueId s e vs2
    return (s, e, FExpr Nothing $ FENE i1 vs2)
oneStepEvaluation queueId s e (FExpr _ (FENE vs1 vs2)) = do
    (s, e, vs1) <- oneStepEvaluation queueId s e vs1
    return (s, e, FExpr Nothing $ FENE vs1 vs2)
oneStepEvaluation queueId s e vs@FSemaphore{} = return (s, e, vs)
oneStepEvaluation queueId s e (FNTValueStatement n (FTValueStatement _ ts)) = return (s, e, takeNth n ts)
oneStepEvaluation queueId s e (FNTValueStatement n vs) = do
    (s, e, vs) <- oneStepEvaluation queueId s e vs
    return (s, e, FNTValueStatement n vs)
oneStepEvaluation queueId s e (FCValueStatement _ name args) = do
    (s, e, FTValueStatement _ args) <- oneStepEvaluation queueId s e (FTValueStatement Nothing args)
    return (s, e, FCValueStatement Nothing name args)
oneStepEvaluation queueId s e (FSusValueStatement vs) = do
    let queueId = getFreeQueueId s
    let nvs = FSuspendedValue queueId
    let ns = putQueue s (QueueT e vs queueId False False)
    return (ns, e, nvs)
oneStepEvaluation queueId s e (FSuspendedValue qId) = do
    s <- runQueue (getQueue qId s) s
    let (QueueT _ vs _ _ _) = getQueue qId s
    return (s, e, vs)


oneStepTupleEvaluation :: Int -> S -> E -> [FValueStatement] -> IO (S, E, [FValueStatement])
oneStepTupleEvaluation _ s e [] = return (s, e, [])
oneStepTupleEvaluation queueId s e (vs:vss) = do
    (s, e, vs_) <- oneStepEvaluation queueId s e vs
    if vs /= vs_
        then return (s, e, vs_:vss)
        else do
            (s, e, vss) <- oneStepTupleEvaluation queueId s e vss
            return (s, e, vs_:vss)

oneStepRunLocs :: Int -> [Int] -> [FValueStatement] -> E -> S -> IO (S, E, FValueStatement)
oneStepRunLocs _ [] _ _ _ = fail "pattern matches not exhaustive"
oneStepRunLocs queueId (x:xs) args env state = do
    let funArgNames = funArgNamesLookup state x
    let (e, vs) = stateLookup x state
    (fits, state, args) <- fitPatternMatchs queueId state env funArgNames args
    if fits
        then 
            if isLambda vs
                then
                    oneStepRunLocsLambda queueId vs args e env state
                else do
                    (e, state) <- registerArgs queueId e env state funArgNames args
                    let nstate = putInLoc x (e, vs) state
                    oneStepRunLoc queueId x nstate
        else oneStepRunLocs queueId xs args env state

oneStepRunLocsLambda _ vs [] innerEnv _ state = return (state, innerEnv, vs)
oneStepRunLocsLambda queueId vs (arg:args) innerEnv outerEnv state = do
    let (FFValueStatement _ name nvs) = vs
    (innerEnv, state) <- registerArgs queueId innerEnv outerEnv state [FPatternMatchB Nothing name] [arg]
    oneStepRunLocsLambda queueId nvs args innerEnv outerEnv state

oneStepRunLoc :: Int -> Int -> S -> IO (S, E, FValueStatement)
oneStepRunLoc queueId x state = do
    let (env, vs) = stateLookup x state
    return (state, env, vs)

registerArgs :: Int -> E -> E -> S -> [FPatternMatch] -> [FValueStatement] -> IO (E, S)
registerArgs queueId innerEnv argEnv state argNames vss = do
    (e, s) <- foldl (registerArgsInFoldF queueId argEnv) (return (innerEnv, state)) $ dList argNames vss
    return (e, s)

registerArgsInFoldF :: Int -> E -> IO (E, S) -> (FPatternMatch, FValueStatement) -> IO (E, S)
registerArgsInFoldF queueId argEnv acc (FPatternMatchB _ str, vs) = do
    (e, s) <- acc
    let (newLoc, newState) = getNewLoc s
    let newEnv = registerLoc False e str newLoc
    let newerState = putInLoc newLoc (argEnv, vs) newState
    return (newEnv, newerState)
registerArgsInFoldF queueId _ acc (FPatternMatchI _ _, _) = acc
registerArgsInFoldF queueId argEnv acc (FPatternMatchC _ _ pms, FCValueStatement _ _ vss) = do
    (e, s) <- acc
    registerArgs queueId e argEnv s pms vss
registerArgsInFoldF queueId argEnv acc a@(FPatternMatchT _ t1, FTValueStatement _ t2) = do
    (e, s) <- acc
    registerArgs queueId e argEnv s t1 t2
registerArgsInFoldF queueId argEnv acc (pm@FPatternMatchT{}, vs) = do
    (innerEnv, state) <- acc
    let (nloc, nstate_) = getNewLoc state
    let nstate = putInLoc nloc (argEnv, vs) nstate_
    let nvs = FAValueStatement Nothing $ FFunApplicationR nloc
    registerArgsInFoldFTuple queueId innerEnv argEnv pm nvs nstate
registerArgsInFoldF queueId argEnv acc (pm@FPatternMatchC{}, vs) = do
    (innerEnv, state) <- acc
    let (nloc, nstate_) = getNewLoc state
    let nstate = putInLoc nloc (argEnv, vs) nstate_
    let nvs = FAValueStatement Nothing $ FFunApplicationR nloc
    registerArgsInFoldFTuple queueId innerEnv argEnv pm nvs nstate

registerArgsInFoldFTuple :: Int -> E -> E -> FPatternMatch -> FValueStatement -> S -> IO (E, S)
registerArgsInFoldFTuple queueId innerEnv argEnv (FPatternMatchB _ x) vs state = do
    let (nloc, nstate) = getNewLoc state
    let nnstate = putInLoc nloc (argEnv, vs) nstate
    let newInnerEnv = registerLoc False innerEnv x nloc
    return (newInnerEnv, nnstate)
registerArgsInFoldFTuple queueId innerEnv argEnv (FPatternMatchT _ ts) vs state = do
    (_, e, s) <- foldl (\es pm -> do {
            (c, e, s) <- es;
            (e, s) <- registerArgsInFoldFTuple queueId e argEnv pm (FNTValueStatement c vs) s; 
            return (c + 1, e, s)
        }) (return (0, innerEnv, state)) ts
    return (e, s)
registerArgsInFoldFTuple _ innerEnv _ FPatternMatchI{} _ state = return (innerEnv, state)
registerArgsInFoldFTuple queueId innerEnv argEnv (FPatternMatchC _ cName cArgs) vs state = do
    (_, e, s) <- foldl (\es pm -> do {
            (c, e, s) <- es;
            (e, s) <- registerArgsInFoldFTuple queueId e argEnv pm (FNTValueStatement c vs) s;
            return (c + 1, e, s)
        }) (return (0, innerEnv, state)) cArgs
    return (e, s)

appendFAVS :: Int -> [Int] -> [FValueStatement] -> S -> IO (E, FValueStatement, Bool, [FPatternMatch], S)
appendFAVS _ [] vss state = fail "Non exhaustive pattern matches"
appendFAVS queueId (loc:xs) addVss state = do
    let argNames = funArgNamesLookup state loc
    let (e, vs) = stateLookup loc state
    (fits, state, addVss) <- fitPatternMatchs queueId state e argNames addVss
    if fits
        then do
            let (appendedVS, b, pms) = appendFAVSInt vs addVss
            return (e, appendedVS, b, pms, state)
        else 
            appendFAVS queueId xs addVss state

appendFAVSInt :: FValueStatement -> [FValueStatement] -> (FValueStatement, Bool, [FPatternMatch])
appendFAVSInt (FAValueStatement _ (FFunApplicationB _ funName vss)) addVss = (FAValueStatement Nothing $ FFunApplicationB Nothing funName (vss ++ addVss), False, [])
appendFAVSInt (FFValueStatement _ name vs) addVSS = (vs, True, [FPatternMatchB Nothing name])
appendFAVSInt _ _ = traceD "shouldn't be here" undefined

fitPatternMatchs :: Int -> S -> E -> [FPatternMatch] -> [FValueStatement] -> IO (Bool, S, [FValueStatement])
fitPatternMatchs _ s e _ [] = return (True, s, [])
fitPatternMatchs _ s e [] vss = return (True, s, vss)
fitPatternMatchs queueId s e (pm:pms) (vs:vss) = do
    (b, s, vs) <- fitPatternMatch queueId s e (pm, vs)
    if b
        then do
            (b_, s, vss) <- fitPatternMatchs queueId s e pms vss
            return (b_, s, vs:vss)
        else return (False, s, vs:vss)
    
wrapArgsInApplR :: [FValueStatement] -> S -> E -> IO ([FValueStatement], S)
wrapArgsInApplR [] s _ = return ([], s)
wrapArgsInApplR (vs:vss) s e = do
    let (nloc, ns) = getNewLoc s
    let nns = putInLoc nloc (e, vs) ns
    (vss, nns) <- wrapArgsInApplR vss nns e
    return (FAValueStatement Nothing (FFunApplicationR nloc):vss, nns)

fitPatternMatch :: Int -> S -> E -> (FPatternMatch, FValueStatement) -> IO (Bool, S, FValueStatement)
fitPatternMatch _ s e (FPatternMatchI _ i1, vs@(FIValueStatement _ i2)) = do
    let (nloc, ns) = getNewLoc s
    let nns = putInLoc nloc (e, vs) ns
    return (i1 == i2, nns, FAValueStatement Nothing $ FFunApplicationR nloc)
fitPatternMatch queueId s e a@(FPatternMatchC _ (FPatternMatchB _ name1) pms, FCValueStatement _ name2 vss) = do
    (res1, s, vss) <- fitPatternMatchs queueId s e pms vss
    (vss, s) <- wrapArgsInApplR vss s e
    return (name1 == name2 && res1, s, FCValueStatement Nothing name2 vss)
fitPatternMatch queueId s e (FPatternMatchB _ _, vs) = do
    let (nloc, ns) = getNewLoc s
    let nns = putInLoc nloc (e, vs) ns
    return (True, nns, FAValueStatement Nothing $ FFunApplicationR nloc)
fitPatternMatch queueId s e (pm@FPatternMatchC{}, vs@FAValueStatement{}) = do
    (s, e, vs) <- oneStepEvaluation queueId s e vs
    fitPatternMatch queueId s e (pm, vs)
fitPatternMatch queueId s e (t@FPatternMatchT{}, vs@FAValueStatement{}) = do
    (s, e, vs) <- oneStepEvaluation queueId s e vs
    fitPatternMatch queueId s e (t, vs)
fitPatternMatch queueId s e a@(t@FPatternMatchT{}, vs@FForceValueStatement{}) = do
    (s, e, vs) <- oneStepEvaluation queueId s e vs
    fitPatternMatch queueId s e (t, vs)
fitPatternMatch queueId s e (FPatternMatchT _ tuple, FTValueStatement _ tuple_) = do
    (fits, s, vss) <- fitPatternMatchs queueId s e tuple tuple_
    let (nloc, ns) = getNewLoc s
    let nns = putInLoc nloc (e, FTValueStatement Nothing vss) ns
    return (fits, nns, FAValueStatement Nothing $ FFunApplicationR nloc)
fitPatternMatch queueId s e (pm , FForceValueStatement _ assignments vs) = do
    (s, e) <- forceRegisterAssignments queueId assignments s e
    fitPatternMatch queueId s e (pm, vs)
fitPatternMatch queueId s e (pm@FPatternMatchI{}, vs) = do
    (s, e, vs) <- oneStepEvaluation queueId s e vs
    fitPatternMatch queueId s e (pm, vs)
fitPatternMatch queueId s e (pm@FPatternMatchT{}, vs) = do
    (s, e, vs) <- oneStepEvaluation queueId s e vs
    fitPatternMatch queueId s e (pm, vs)
fitPatternMatch queueId s e (pm@FPatternMatchC{}, vs) = do
    (s, e, vs) <- oneStepEvaluation queueId s e vs
    fitPatternMatch queueId s e (pm, vs)

wrapFunctionB :: Int -> FValueStatement -> E -> S -> IO (S, FValueStatement)
wrapFunctionB queueId vs@(FAValueStatement _ (FFunApplicationB _ x vss)) env state = do
    let locs = lookupLoc x env
    wrapFunctionBInt queueId locs vs env state
wrapFunctionB _ _ _ _ = traceD "shouldn't be here" undefined

wrapFunctionBInt :: Int -> [Int] -> FValueStatement -> E -> S -> IO (S, FValueStatement)
wrapFunctionBInt _ [] (FAValueStatement _ (FFunApplicationB _ name _)) _ _ = fail $ "pattern match not exhaustive for function " ++ name
wrapFunctionBInt queueId (loc:locs) vs@(FAValueStatement _ (FFunApplicationB _ x vss)) env state = do
    let funArgNames = funArgNamesLookup state loc
    (fits, state, vss) <- fitPatternMatchs queueId state env funArgNames vss
    if fits
        then do
            let d = length funArgNames - length vss
            let (newLocs, newState) = getNNewLocs state d
            let newLambdas = map show newLocs
            return (newState, wrapFunctionBIntNNewLambdas d vs newLambdas)
        else 
            wrapFunctionBInt queueId locs vs env state
wrapFunctionBInt _ _ _ _ _ = traceD "shouldn't be here" undefined

wrapFunctionBIntNNewLambdas :: Int -> FValueStatement -> [String] -> FValueStatement
wrapFunctionBIntNNewLambdas d (FAValueStatement _ (FFunApplicationB _ x vss)) locs = 
    let
        argVss = map makeFunApplicationNoArg locs
    in foldl (flip $ FFValueStatement Nothing) (FAValueStatement Nothing (FFunApplicationB Nothing x (vss ++ argVss))) locs
wrapFunctionBIntNNewLambdas _ _ _ = traceD "shouldn't be here" undefined

makeFunApplicationNoArg :: String -> FValueStatement
makeFunApplicationNoArg x = FAValueStatement Nothing (FFunApplicationB Nothing x [])