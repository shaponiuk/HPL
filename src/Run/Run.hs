module Run.Run where

import StaticCheck.Format
import Util.Util
import Util.Env
import Util.State

run :: NProgramFormat -> IO ()
run (NSIT structs interfaces algTypes state) = do
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
        (t, vs) = stateLookup loc state
    in let
        maybeIOFun = interpretVS vs env [] state []
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
interpretVS vs env argNames s vss = do
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
runVS (FAValueStatement (FFunApplicationB funName funArgVss)) env state = do
    let loc = lookupFirstLoc funName env
    let (_, vs) = stateLookup loc state
    let funArgNames = funArgNamesLookup state loc
    if length funArgVss < length funArgNames
        then 
            -- gets new locs as ids and wraps the function in lambdas so that (number of args -- lambda wraps) == (length funArgVss)
            return $ Just $ wrapFunction vs funArgNames funArgVss env state
        else 
            if length funArgVss > length funArgNames
                then do
                    print "heeereee"
                    print funArgNames
                    print funArgVss
                    print vs
                    let updatedVS = appendFAVS vs funArgVss
                    -- here need to do some trickery to swap eg. the x for an original function and interpretVS it again recursively, until it is done right
                    -- interpretVS vs env funArgNamesNew state funArgVss
                    runVS updatedVS env state
                else
                    interpretVS vs env funArgNames state funArgVss
runVS (FExpr (FEMul vs1 vs2)) env state = do
    Just (newState, FIValueStatement i1) <- runVS vs1 env state
    Just (newerState, FIValueStatement i2) <- runVS vs2 env newState
    return $ Just (newerState, FIValueStatement (i1 * i2))
runVS _ _ _ = undefined

appendFAVS :: FValueStatement -> [FValueStatement] -> FValueStatement
appendFAVS (FAValueStatement (FFunApplicationB funName vss)) addVss = FAValueStatement $ FFunApplicationB funName (vss ++ addVss)
appendFAVS _ _ = undefined

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
    let newEnv = registerLoc env x loc
    let newerState = putInLoc loc (t, vs) newState
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
forceRunFunApplication _ _ _ = undefined

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
        newEnv = registerLoc e str newLoc
        newerState = putInLoc newLoc (FTypeT [], vs) newState
    in (newEnv, newerState)
registerArgsInFoldF _ _ = undefined