module Run.Run where

import StaticCheck.Format
import Util.Util
import Util.Env
import Util.State
import Debug.Trace

run :: NProgramFormat -> IO ()
run (NSIT structs state) = do
    print state
    let mainStruct = getMainStruct structs
    let (mainName, mainArgs, env) = getMainFunction mainStruct
    s <- runFunction (mainName, mainArgs, env) [] state
    print s

getMainStruct :: [NFStruct] -> NFStruct
getMainStruct = head . filter (\(NFStruct name _ _) -> name == "Main")

getMainFunction :: NFStruct -> (String, [FPatternMatch], E)
getMainFunction (NFStruct _ _ (NFStructBody _ l _ _ _ _)) =
    (\(NFNonSusFunDef name args env) -> (name, args, env)) $ head $ filter (\(NFNonSusFunDef name _ _) -> name == "main") l

runFunction :: (String, [FPatternMatch], E) -> [FValueStatement] -> S -> IO (S, FValueStatement)
runFunction p@(name, args, env) vss state =
    let locs = lookupLoc name env in tryLocs p vss state locs

tryLocs :: (String, [FPatternMatch], E) -> [FValueStatement] -> S -> [Int] -> IO (S, FValueStatement)
tryLocs _ _ _ [] = fail "EEE"
tryLocs (_, _, env) _ state (loc:_) =
    let
        (e, _, vs) = stateLookup loc state
    in let
        maybeIOFun = interpretVS vs e [] state []
    in runMaybeIOFun maybeIOFun

runMaybeIOFun :: IO (Maybe (S, FValueStatement)) -> IO (S, FValueStatement)
runMaybeIOFun x = do
    x_ <- x
    runMaybeIOFunInt x_

runMaybeIOFunInt :: Maybe (S, FValueStatement) -> IO (S, FValueStatement)
runMaybeIOFunInt Nothing = 
    fail "UUUUUU"
runMaybeIOFunInt (Just x) = do
    print "HAHAHAHAHA"
    return x

interpretVS :: FValueStatement -> E -> [FPatternMatch] -> FunRunT
interpretVS vs env argNames s vss =
    if length vss < length argNames
        then
            return $ Just $ wrapFunction vs argNames vss env s
        else
            let (newEnv, newState) = registerArgs env s argNames vss in 
                runVS vs newEnv newState

runVS :: FValueStatement -> E -> FunRunQuickT
runVS (FForceValueStatement assignments vs) env state = do
    (newState, newEnv) <- forceRegisterAssignments assignments state env
    seq newState $ runVS vs newEnv newState
runVS vs@(FIValueStatement i) _ s = return $ Just (s, vs)
runVS (FAValueStatement (FFunApplicationB funName funArgVss)) env oldState = do
    print "hereFA"
    print funName
    let firstLoc = lookupFirstLoc funName env
    let locs = lookupLoc funName env
    let funArgNames = funArgNamesLookup oldState firstLoc
    (state, almostComputedVss) <- foldl (runVSInFoldF env) (return (oldState, [])) funArgVss
    let computedVss = reverse almostComputedVss
    if length funArgVss > length funArgNames
        then do
            let (e, updatedVS) = forceUnwrapMaybe $ appendFAVS locs computedVss state
            runVS updatedVS e state
        else
            tryRunVSFunApplR locs computedVss state
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
runVS (FAValueStatement (FFunApplicationR locs args)) _ state = do
    print "hereFC"
    print locs
    tryRunVSFunApplR locs args state
runVS vs@(FLitStrValueStatement _) _ s = return $ Just (s, vs)
runVS (FCValueStatement name vss) e s = do
    (new_state, newVss) <- foldl (cvsInFoldF e) (return (s, [])) vss
    return $ Just (new_state, FCValueStatement name (reverse newVss))

runVS vs _ _ = trace (show vs) undefined

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
    let (env, _, vs) = stateLookup x state
    let argNames = funArgNamesLookup state x
    print "here"
    print argNames
    print args
    if fitPatternMatchs argNames args
        then do
            interpretVS vs env argNames state args
        else 
            tryRunVSFunApplR xs args state

fitPatternMatchs :: [FPatternMatch] -> [FValueStatement] -> Bool
fitPatternMatchs pms vss = all fitPatternMatch $ dList pms vss

fitPatternMatch :: (FPatternMatch, FValueStatement) -> Bool
fitPatternMatch (FPatternMatchI i1, FIValueStatement i2) = i1 == i2

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
convertBApplicationsToRApplications a@(FIValueStatement i) _ = a

appendFAVS :: [Int] -> [FValueStatement] -> S -> Maybe (E, FValueStatement)
appendFAVS [] vss state = undefined -- todo: error
appendFAVS (loc:xs) addVss state =
    let
        argNames = funArgNamesLookup state loc
        (e, _, vs) = stateLookup loc state
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
    (vss, newState, newEnv) <- forceGetTupleVSS vs state env
    foldl setPMInFoldF (return (newState, newEnv)) $ tList types pmL vss
setPM t (FPatternMatchB x) vs state env = do
    let (loc, newState) = getNewLoc state
    let newEnv = registerLoc False env x loc
    let newerState = putInLoc loc (newEnv, t, vs) newState
    return (newerState, newEnv)
setPM _ _ _ _ _ = undefined

setPMInFoldF :: IO (S, E) -> (FType, FPatternMatch, FValueStatement) -> IO (S, E)
setPMInFoldF acc (t, pm, vs) = do
    (s, e) <- acc
    setPM t pm vs s e

forceGetTupleVSS :: FValueStatement -> S -> E -> IO ([FValueStatement], S, E)
forceGetTupleVSS (FTValueStatement vss) s e = return (vss, s, e)
forceGetTupleVSS (FAValueStatement funApplication) state env = do
    (vs, s, e) <- forceRunFunApplication funApplication state env
    forceGetTupleVSS vs s e
forceGetTupleVSS _ _ _ = undefined

forceRunFunApplication :: FFunApplication -> S -> E -> IO (FValueStatement, S, E)
forceRunFunApplication (FFunApplicationB "print" [str]) state env = do
    interpretedArgMaybe <- interpretVS str env [] state []
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
        newerState = putInLoc newLoc (newEnv, FTypeT [], vs) newState
    in (newEnv, newerState)
registerArgsInFoldF acc (FPatternMatchI _, _) = acc
registerArgsInFoldF _ _ = undefined