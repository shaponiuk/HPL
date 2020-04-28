module Run.Run where

import StaticCheck.Format

run :: NProgramFormat -> IO ()
run (NSIT structs interfaces algTypes state) = do
    print structs
    pure ()

interpretVS :: FValueStatement -> E -> [String] -> FunRunT
interpretVS vs env argNames =
    (\s vss ->
        let
            (newEnv, newState) = registerArgs env s argNames vss
        in runVS vs newEnv newState
    )

runVS :: FValueStatement -> E -> FunRunQuickT
runVs vs env = undefined

registerArgs :: E -> S -> [String] -> [FValueStatement] -> (E, S)
registerArgs env state argNames vss = 
    foldl (\(e, s) (str, vs)
    ) (env, state) $ dList argNames vss

dList [] [] = []
dList (x:xs) (y:ys) = (x, y) ++ (dList xs ys)