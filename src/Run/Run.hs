module Run.Run where

import StaticCheck.Format
import Util.Util
import Util.Env
import Util.State
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
    print s
    print vsf

getMainStruct :: [NFStruct] -> NFStruct
getMainStruct = head . filter (\(NFStruct name _ _) -> name == "Main")

getMainFunction :: NFStruct -> (String, [FPatternMatch], E)
getMainFunction (NFStruct _ _ (NFStructBody _ l _ _ _ _)) =
    (\(NFNonSusFunDef name args env) -> (name, args, env)) $ head $ filter (\(NFNonSusFunDef name _ _) -> name == "main") l

interpretVS :: FValueStatement -> E -> [FPatternMatch] -> Int -> FunRunT
interpretVS vs env argNames loc s vss = do
    Just (ns, nvs) <- if length vss < length argNames
        then
            let (s_, vs) = wrapFunction vs argNames vss env s in
                runVS vs env s_
        else
            let (newEnv, newState) = registerArgs env s argNames vss in 
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
                    let (e, updatedVS) = forceUnwrapMaybe $ appendFAVS locs computedVss state
                    runVS updatedVS e state
                else
                    tryRunVSFunApplR locs computedVss state
        else interpretVS vs env funArgNames firstLoc oldState funArgVss
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

oneStepEvaluation :: FValueStatement -> E -> S -> FValueStatement
oneStepEvaluation = undefined

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
    if fitPatternMatchs argNames args
        then
            let (newEnv, newState) = registerArgs env state argNames args in 
                runVS vs newEnv newState
        else 
            tryRunVSFunApplR xs args state

fitPatternMatchs :: [FPatternMatch] -> [FValueStatement] -> Bool
fitPatternMatchs pms vss = all fitPatternMatch $ dList pms vss

fitPatternMatch :: (FPatternMatch, FValueStatement) -> Bool
fitPatternMatch (FPatternMatchI i1, FIValueStatement i2) = i1 == i2
fitPatternMatch a@(FPatternMatchC (FPatternMatchB name1) pms, FCValueStatement name2 vss) =
    trace (show a) $ name1 == name2 && fitPatternMatchs pms vss
fitPatternMatch (FPatternMatchB _, _) = True
-- fitPatternMatch (FPatternMatchC (FPatternMatchB name1) pms, FAValueStatement (FFunApplicationR loc argVss)) = -- do one step evaluation and try to fit again
-- fitPatternMatch a = trace ("fitPatternMatch undefined " ++ show a) undefined

runVSInFoldF :: E -> IO (S, [FValueStatement]) -> FValueStatement -> IO (S, [FValueStatement])
runVSInFoldF env acc vs = do
    (s, vsl) <- acc
    -- Just (ns, nvs) <- runVS vs env s
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
convertBApplicationsToRApplications a b = trace (show a) undefined

appendFAVS :: [Int] -> [FValueStatement] -> S -> Maybe (E, FValueStatement)
appendFAVS [] vss state = undefined -- todo: error
appendFAVS (loc:xs) addVss state =
    let
        argNames = funArgNamesLookup state loc
        (f, e, _, vs) = stateLookup loc state
    in if fitPatternMatchs argNames addVss
        then
            let
                appendedVS = appendFAVSInt vs addVss
            in Just (e, appendedVS)
        else appendFAVS xs addVss state

appendFAVSInt (FAValueStatement (FFunApplicationB funName vss)) addVss = FAValueStatement $ FFunApplicationB funName (vss ++ addVss)
appendFAVSInt _ _ = undefined

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
registerAssignment _ _ _ = undefined

setPM :: FType -> FPatternMatch -> FValueStatement -> S -> E -> IO (S, E)
setPM (FTypeT types) (FPatternMatchT pmL) vs state env = do
    print "heeeerrreeeee"
    (vss, newState, newEnv) <- forceGetTupleVSS vs state env
    print "heeeeerrrreeeee"
    print pmL
    print vss
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
    print "watch_out"
    print e_
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
forceRunFunApplication a@(FFunApplicationB name args) state env = do
    Just (s, vs) <- runVS (FAValueStatement a) env state
    return (vs, s, env)
forceRunFunApplication a@(FFunApplicationR loc [args]) state env = do
    Just (s, vs) <- runVS (FAValueStatement a) env state
    return (vs, s, env)
forceRunFunApplication a _ _ = trace (show a) undefined

forceUnwrapMaybe :: Maybe a -> a
forceUnwrapMaybe (Just x) = x
forceUnwrapMaybe Nothing = undefined

registerArgs :: E -> S -> [FPatternMatch] -> [FValueStatement] -> (E, S)
registerArgs env state argNames vss = 
    result where
        result = foldl registerArgsInFoldF (env, state) $ dList argNames vss

registerArgsInFoldF :: (E, S) -> (FPatternMatch, FValueStatement) -> (E, S)
registerArgsInFoldF (e, s) (FPatternMatchB str, vs) =
    let
        (newLoc, newState) = getNewLoc s
        newEnv = registerLoc False e str newLoc
        newerState = putInLoc newLoc (False, newEnv, FTypeT [], vs) newState
    in (newEnv, newerState)
registerArgsInFoldF acc (FPatternMatchI _, _) = acc
registerArgsInFoldF (e, s) (FPatternMatchC _ pms, FCValueStatement _ vss) = 
    registerArgs e s pms vss
-- registerArgsInFoldF (e, s) (FPatternMatchC _ pms, FAValueStatement (FFunApplicationR loc argVss)) = do
    -- one step evaluation, do every step until registerArgs matches
registerArgsInFoldF _ el = trace ("registerArgsInFoldF " ++ show el) undefined