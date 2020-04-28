module Run.Run where

import StaticCheck.Format

run :: NProgramFormat -> IO ()
run (NSIT structs interfaces algTypes state) = do
    print structs
    let mainStruct = getMainStruct structs
    let (mainName, mainArgs, env) = getMainFunction mainStruct
    runFunction (mainName, mainArgs, env) [] state

getMainStruct :: [NFStruct] -> NFStruct
getMainStruct = head . (filter (\(NFStruct name _ _) -> name == "Main"))

getMainFunction :: NFStruct -> (String, [FFunctionArg], E)
getMainFunction (NFStruct _ _ (NFStructBody _ l _ _ _ _)) =
    (\(NFNonSusFunDef name args env) -> (name, args, env)) $ head $ filter (\(NFNonSusFunDef name _ _) -> name == "main") l

runFunction :: (String, [FFunctionArg], E) -> [FValueStatement] -> S -> IO ()
runFunction p@(name, args, env) vss state = do
    print "heeree"
    let locs = lookupLoc name env in tryLocs p vss state locs

tryLocs :: (String, [FFunctionArg], E) -> [FValueStatement] -> S -> [Int] -> IO ()
tryLocs _ _ _ [] = print "EEE"
tryLocs (name, _, env) vss state (loc:_) =
    let
        (t, vs) = stateLookup loc state
    in let
        maybeIOFun = interpretVS vs env [] state []
    in runMaybeIOFun maybeIOFun

runMaybeIOFun :: IO (Maybe (S, FValueStatement)) -> IO ()
runMaybeIOFun x = do
    x_ <- x
    runMaybeIOFunInt x_

runMaybeIOFunInt :: Maybe (S, FValueStatement) -> IO ()
runMaybeIOFunInt Nothing = print "UUUUUU"
runMaybeIOFunInt (Just x) = print "HAHAHAHAHA"

interpretVS :: FValueStatement -> E -> [String] -> FunRunT
interpretVS vs env argNames =
    (\s vss ->
        let
            (newEnv, newState) = registerArgs env s argNames vss
        in runVS vs newEnv newState
    )

runVS :: FValueStatement -> E -> FunRunQuickT
runVS (FForceValueStatement assignments vs) env state = do
    (newState, newEnv) <- forceRegisterAssignments assignments state env
    seq newState $ runVS vs newEnv newState
runVS vs@(FIValueStatement i) _ s = return $ Just (s, vs)
runVS _ _ _ = undefined

forceRegisterAssignments :: [FAssignment] -> S -> E -> IO (S, E)
forceRegisterAssignments assignments state env =
    foldl forceRegisterAssignmentsInFoldF (return (state, env)) assignments

forceRegisterAssignmentsInFoldF :: IO (S, E) -> FAssignment -> IO (S, E)
forceRegisterAssignmentsInFoldF acc assignment = do
    (s, e) <- acc
    registerAssignment assignment s e

registerAssignment :: FAssignment -> S -> E -> IO (S, E)
registerAssignment (FAssignmentB t pm vs) state env = 
    setPM pm vs state env
registerAssignment _ _ _ = undefined

setPM :: FPatternMatch -> FValueStatement -> S -> E -> IO (S, E)
setPM (FPatternMatchT pmL) vs state env = do
    (vss, newState, newEnv) <- forceGetTupleVSS vs state env
    foldl setPMInFoldF (return (newState, newEnv)) $ dList pmL vss
setPM _ _ _ _ = undefined

setPMInFoldF :: IO (S, E) -> (FPatternMatch, FValueStatement) -> IO (S, E)
setPMInFoldF acc (pm, vs) = do
    (s, e) <- acc
    setPM pm vs s e

forceGetTupleVSS :: FValueStatement -> S -> E -> IO ([FValueStatement], S, E)
forceGetTupleVSS (FTValueStatement vss) s e = return (vss, s, e)
forceGetTupleVSS (FAValueStatement funApplication) state env = do
    (vs, s, e) <- forceRunFunApplication funApplication state env
    forceGetTupleVSS vs s e
forceGetTupleVSS _ _ _ = undefined

forceRunFunApplication :: FFunApplication -> S -> E -> IO (FValueStatement, S, E)
forceRunFunApplication (FFunApplicationB "print" [str]) state env = do
    print str
    return (FTValueStatement [], state, env)
forceRunFunApplication _ _ _ = undefined

registerArgs :: E -> S -> [String] -> [FValueStatement] -> (E, S)
registerArgs env state argNames vss = 
    foldl (\(e, s) (str, vs) ->
        let
            (newLoc, newState) = getNewLoc s
            newEnv = registerLoc e str newLoc
        in (newEnv, newState)
    ) (env, state) $ dList argNames vss

dList :: [a] -> [b] -> [(a, b)]
dList [] [] = []
dList (x:xs) (y:ys) = (x, y):(dList xs ys)