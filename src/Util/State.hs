module Util.State where

import Data.Map
import StaticCheck.Format
import Debug.Trace

getNewLoc :: S -> (Int, S)
getNewLoc (S varsMap loc funArgs semaphores queues) = (loc + 1, S varsMap (loc + 1) funArgs semaphores queues)

putInLoc :: Int -> (Bool, E, FType, FValueStatement) -> S -> S
putInLoc loc thing (S varsMap newIntLoc funArgs semaphores queues) =
  S (insert loc thing varsMap) newIntLoc funArgs semaphores queues

stateLookup :: Int -> S -> (Bool, E, FType, FValueStatement)
stateLookup loc (S varsMap _ _ _ _) = 
  if member loc varsMap
    then varsMap ! loc
    else trace ("loc not found in state " ++ show loc) undefined

funArgNamesLookup :: S -> Int -> [FPatternMatch]
funArgNamesLookup (S _ _ funArgs _ _) loc =
  if member loc funArgs
    then funArgs ! loc
    else []

putArgNames :: S -> Int -> [FPatternMatch] -> S
putArgNames (S varsMap newIntLoc functionArgsMap semaphores queues) loc strs = 
  S varsMap newIntLoc (insert loc strs functionArgsMap) semaphores queues

getUnsetLoc :: S -> [Int] -> Int
getUnsetLoc _ [] = undefined
getUnsetLoc s (x:xs) = if member x (vars s) then getUnsetLoc s xs else x

getFreeQueueId :: S -> Int
getFreeQueueId (S _ _ _ _ queues) = length queues

putQueue :: S -> (E, FValueStatement, Int) -> S
putQueue (S varsMap newIntLoc functionArgsMap semaphores queues) thing =
  S varsMap newIntLoc functionArgsMap semaphores (queues ++ [thing])

anyAvailibleQueue :: S -> Bool
anyAvailibleQueue (S _ _ _ semaphores queues) = any (`blockedByAnyOfTheSemaphores` semaphores) queues

blockedByAnyOfTheSemaphores :: (E, FValueStatement, Int) -> [([Int], Int)] -> Bool
blockedByAnyOfTheSemaphores q = any (q `blockedBySemaphore`)

blockedBySemaphore :: (E, FValueStatement, Int) -> ([Int], Int) -> Bool
blockedBySemaphore (_, _, queueId) (blockedQueues, _) = queueId `elem` blockedQueues

getAvailibleQueue :: S -> (E, FValueStatement, Int)
getAvailibleQueue = undefined

getNewState :: S
getNewState = S empty 0 empty [] []