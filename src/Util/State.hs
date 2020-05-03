module Util.State where

import Data.Map
import StaticCheck.Format
import Util.Util
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

putQueue :: S -> (E, FValueStatement, Int, Bool) -> S
putQueue (S varsMap newIntLoc functionArgsMap semaphores queues) thing =
  S varsMap newIntLoc functionArgsMap semaphores (queues ++ [thing])

anyAvailibleQueue :: S -> Bool
anyAvailibleQueue (S _ _ _ semaphores queues) = any (`notBlockedByAnyOfTheSemaphores` semaphores) queues

notBlockedByAnyOfTheSemaphores :: (E, FValueStatement, Int, Bool) -> [([Int], Int, Int)] -> Bool
notBlockedByAnyOfTheSemaphores (_, _, _, False) [] = True
notBlockedByAnyOfTheSemaphores (_, _, _, True) [] = False
notBlockedByAnyOfTheSemaphores q semaphores = any (q `notBlockedBySemaphoreOrNotFinished`) semaphores

checkNotBlocked :: (E, FValueStatement, Int, Bool) -> S -> Bool
checkNotBlocked queue (S _ _ _ semaphores _) = notBlockedByAnyOfTheSemaphores queue semaphores

notBlockedBySemaphoreOrNotFinished :: (E, FValueStatement, Int, Bool) -> ([Int], Int, Int) -> Bool
notBlockedBySemaphoreOrNotFinished (_, _, queueId, b) (blockedQueues, _, _) = queueId `notElem` blockedQueues || not b

getAvailibleQueue :: S -> (E, FValueStatement, Int, Bool)
getAvailibleQueue (S _ _ _ semaphores queues) = first (`notBlockedByAnyOfTheSemaphores` semaphores) queues

putInQueue :: Int -> (E, FValueStatement, Int, Bool) -> S -> S
putInQueue queueId q (S varsMap newIntLoc functionArgsMap semaphores queues) =
  let
    updatedQueues = updateQueues queueId q queues
  in S varsMap newIntLoc functionArgsMap semaphores updatedQueues

updateQueues :: Int -> (E, FValueStatement, Int, Bool) -> [(E, FValueStatement, Int, Bool)] -> [(E, FValueStatement, Int, Bool)]
updateQueues qId q [] = []
updateQueues qId q ((e, vs, qId_, b):queues) = if qId == qId_ then q:queues else (e, vs, qId_, b):updateQueues qId q queues

getQueue :: Int -> S -> (E, FValueStatement, Int, Bool)
getQueue qId (S _ _ _ _ queues) = first (\(_, _, id, _) -> id == qId) queues

getNewSemaphore :: S -> (([Int], Int, Int), S)
getNewSemaphore (S varsMap newIntLoc functionArgsMap semaphores queues) =
  let
    newSemId = length semaphores
  in (([], 0, newSemId), S varsMap newIntLoc functionArgsMap (semaphores ++ [([], 0, newSemId)]) queues)

getSemaphore :: Int -> S -> ([Int], Int, Int)
getSemaphore = undefined

putSemaphore :: Int -> ([Int], Int, Int) -> S -> S
putSemaphore = undefined

getNewState :: S
getNewState = S empty 0 empty [] []