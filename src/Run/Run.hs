module Run.Run where

import StaticCheck.Format
import Util.Util
import Util.Env
import Util.State
import Run.OneStepEvaluation
import Debug.Trace

run :: NProgramFormat -> IO ()
run (NSIT structs state) = do
    let (nloc, nstate) = getNewLoc state
    let (_, _, env) = getMainFunction $ getMainStruct structs
    let mainloc = lookupFirstLoc "main" env
    let (_, nenv, t, vs) = stateLookup mainloc nstate
    let nnstate = putInLoc nloc (False, nenv, t, vs) nstate
    Just (s, vsf) <- interpretVS vs nenv [] nloc nnstate []
    print "Finish"

getMainStruct :: [NFStruct] -> NFStruct
getMainStruct = head . filter (\(NFStruct name _ _) -> name == "Main")

getMainFunction :: NFStruct -> (String, [FPatternMatch], E)
getMainFunction (NFStruct _ _ (NFStructBody _ l _ _ _ _)) =
    (\(NFNonSusFunDef name args env) -> (name, args, env)) $ head $ filter (\(NFNonSusFunDef name _ _) -> name == "main") l

interpretVS :: FValueStatement -> E -> [FPatternMatch] -> Int -> FunRunT
interpretVS vs env argNames loc s vss =
    if length vss < length argNames
        then
            let (s_, vs) = wrapFunction vs argNames vss env s in
                runVS vs env s_
        else do
            Just (ns, nvs) <- let (newEnv, newState) = registerArgs env s argNames vss in 
                runVS vs newEnv newState
            let (b, t, e, _) = stateLookup loc ns
            let nns = putInLoc loc (b, t, e, nvs) ns
            return $ Just (nns, nvs)

runVS :: FValueStatement -> E -> FunRunQuickT
runVS (FForceValueStatement assignments vs) env state = do
    (newState, newEnv) <- forceRegisterAssignments assignments state env
    seq newState $ runVS vs newEnv newState
runVS vs@(FIValueStatement i) _ s = return $ Just (s, vs)
runVS (FAValueStatement (FFunApplicationB funName funArgVss)) env oldState = do
    let firstLoc = lookupFirstLoc funName env
    let locs = lookupLoc funName env
    let funArgNames = funArgNamesLookup oldState firstLoc
    let (ifFunction, _, _, vs) = stateLookup firstLoc oldState
    if ifFunction
        then do
            (state, almostComputedVss) <- foldl (runVSInFoldF env) (return (oldState, [])) funArgVss
            let computedVss = reverse almostComputedVss
            if length funArgVss > length funArgNames
                then do
                    let (e, updatedVS, b, pms) = forceUnwrapMaybe $ appendFAVS locs computedVss state
                    if b
                        then
                            let (newEnv, newState) = registerArgs e state pms computedVss in 
                                runVS updatedVS newEnv newState
                        else
                            runVS updatedVS e state
                else
                    tryRunVSFunApplR locs computedVss state
        else if length funArgVss > length funArgNames
            then do
                (state, almostComputedVss) <- foldl (runVSInFoldF env) (return (oldState, [])) funArgVss
                let computedVss = reverse almostComputedVss
                let (e, updatedVS, b, pms) = forceUnwrapMaybe $ appendFAVS locs computedVss state
                if b
                    then
                        let (newEnv, newState) = registerArgs e state pms computedVss in 
                            runVS updatedVS newEnv newState
                    else
                        runVS updatedVS e state
            else
                interpretVS vs env funArgNames firstLoc oldState funArgVss
runVS (FExpr (FEMul vs1 vs2)) env state = do
    Just (newState, FIValueStatement i1) <- runVS vs1 env state
    Just (newerState, FIValueStatement i2) <- runVS vs2 env newState
    return $ Just (newerState, FIValueStatement (i1 * i2))
runVS (FIfValueStatement condvs res1vs res2vs) env state = do
    Just (newState, FIValueStatement condVal) <- runVS condvs env state
    if condVal /= 0
        then runVS res1vs env newState
        else runVS res2vs env newState
runVS (FExpr (FEEQ vs1 vs2)) env state = do
    Just (newState, FIValueStatement i1) <- runVS vs1 env state
    Just (newerState, FIValueStatement i2) <- runVS vs2 env newState
    return $ Just (newerState, FIValueStatement $ if i1 == i2 then 1 else 0)
runVS (FExpr (FESub vs1 vs2)) env state = do
    Just (newState, FIValueStatement i1) <- runVS vs1 env state
    Just (newerState, FIValueStatement i2) <- runVS vs2 env newState
    return $ Just (newerState, FIValueStatement (i1 - i2))
runVS (FExpr (FEAdd vs1 vs2)) env state = do
    Just (newState, FIValueStatement i1) <- runVS vs1 env state
    Just (newerState, FIValueStatement i2) <- runVS vs2 env newState
    return $ Just (newerState, FIValueStatement (i1 + i2))
runVS (FAValueStatement (FFunApplicationR locs args)) _ state =
    tryRunVSFunApplR locs args state
runVS vs@(FLitStrValueStatement _) _ s = return $ Just (s, vs)
runVS (FCValueStatement name vss) e s = do
    (new_state, newVss) <- foldl (cvsInFoldF e) (return (s, [])) vss
    return $ Just (new_state, FCValueStatement name (reverse newVss))
runVS (FTValueStatement xss) e s = do
    (ns, xssc) <- foldl (runTVSInFoldF e) (return (s, [])) xss
    return $ Just (ns, FTValueStatement xssc)

runVS vs _ _ = trace ("??? " ++ show vs) undefined

runTVSInFoldF :: E -> IO (S, [FValueStatement]) -> FValueStatement -> IO (S, [FValueStatement])
runTVSInFoldF env acc vs = do
    (s, vsl) <- acc
    Just (ns, nvs) <- runVS vs env s
    return (ns, nvs:vsl)

cvsInFoldF :: E -> IO (S, [FValueStatement]) -> FValueStatement -> IO (S, [FValueStatement])
cvsInFoldF e acc vs = do
    (state, vsl) <- acc
    Just (new_state, vs_) <- runVS vs e state
    return (new_state, vs_:vsl)

tryRunVSFunApplR :: [Int] -> [FValueStatement] -> FunRunQuickT
    -- todo: here might fail
tryRunVSFunApplR [] _ _ = undefined
-- todo: hard to find the function name, probably unneeded, only one at the time
tryRunVSFunApplR (x:xs) args state = do
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
                                runVS updatedVS newEnv newState
                        else 
                            runVS updatedVS e state
                else
                    let (newEnv, newState) = registerArgs env state argNames args in 
                        runVS vs newEnv newState
        else 
            tryRunVSFunApplR xs args state

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
wrapFunction = undefined

forceRegisterAssignments :: [FAssignment] -> S -> E -> IO (S, E)
forceRegisterAssignments assignments state env =
    foldl forceRegisterAssignmentsInFoldF (return (state, env)) assignments

forceRegisterAssignmentsInFoldF :: IO (S, E) -> FAssignment -> IO (S, E)
forceRegisterAssignmentsInFoldF acc assignment = do
    (s, e) <- acc
    registerAssignment assignment s e

registerAssignment :: FAssignment -> S -> E -> IO (S, E)
registerAssignment (FAssignmentB t pm vs) state env = 
    setPM t pm vs state env
registerAssignment (FRefAssignment (FRefDef t name vs)) state env = do
    let (refLoc, newState) = getNewLoc state
    let (vsLoc, newerState) = getNewLoc newState
    let refVS = FRefAddr vsLoc
    let newEnv = registerLoc False env name refLoc
    let newererState = putInLoc refLoc (False, newEnv, FTypeB "Ref" [t], refVS) newerState
    let newerererState = putInLoc vsLoc (False, newEnv, t, vs) newererState
    return (newerererState, newEnv)

setPM :: FType -> FPatternMatch -> FValueStatement -> S -> E -> IO (S, E)
setPM (FTypeT types) (FPatternMatchT pmL) vs state env = do
    (vss, newState, newEnv) <- forceGetTupleVSS vs state env
    foldl setPMInFoldF (return (newState, newEnv)) $ tList types pmL vss
setPM t (FPatternMatchB x) vs state env = do
    let (loc, newState) = getNewLoc state
    let newEnv = registerLoc False env x loc
    let newerState = putInLoc loc (False, newEnv, t, vs) newState
    return (newerState, newEnv)
setPM _ _ _ _ _ = undefined

setPMInFoldF :: IO (S, E) -> (FType, FPatternMatch, FValueStatement) -> IO (S, E)
setPMInFoldF acc (t, pm, vs) = do
    (s, e) <- acc
    setPM t pm vs s e

unwrapSingleTuples :: FValueStatement -> [FValueStatement]
unwrapSingleTuples (FTValueStatement [vs]) = unwrapSingleTuples vs
unwrapSingleTuples (FTValueStatement vss) = vss
unwrapSingleTuples a = [a]

forceGetTupleVSS :: FValueStatement -> S -> E -> IO ([FValueStatement], S, E)
forceGetTupleVSS a@(FTValueStatement vss) s e = do
    Just (_, e_) <- runVS a e s
    Just (ns, tvs) <- runVS a e s
    return (unwrapSingleTuples tvs, ns, e)
forceGetTupleVSS (FAValueStatement funApplication) state env = do
    (vs, s, e) <- forceRunFunApplication funApplication state env
    forceGetTupleVSS vs s e
forceGetTupleVSS _ _ _ = undefined

forceRunFunApplication :: FFunApplication -> S -> E -> IO (FValueStatement, S, E)
forceRunFunApplication (FFunApplicationB "print" [str]) state env = do
    interpretedArgMaybe <- runVS str env state
    let (s, r)  = forceUnwrapMaybe interpretedArgMaybe
    print $ "hehe " ++ show r
    return (FTValueStatement [], s, env)
forceRunFunApplication vs@(FFunApplicationB "set" [ref, value]) state env = do
    Just (newState, FRefAddr refAddr) <- runVS ref env state
    let newerState = putInLoc refAddr (False, env, FTypeT [], value) newState
    return (FTValueStatement [], newerState, env)
forceRunFunApplication a@(FFunApplicationB name args) state env = do
    Just (s, vs) <- runVS (FAValueStatement a) env state
    return (vs, s, env)
forceRunFunApplication a@(FFunApplicationR loc [args]) state env = do
    Just (s, vs) <- runVS (FAValueStatement a) env state
    return (vs, s, env)
forceRunFunApplication a _ _ = trace ("forceRunFunApplication " ++ show a) undefined