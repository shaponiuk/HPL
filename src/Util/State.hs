module Util.State where

import Data.Map
import StaticCheck.Format
import Util.Util
import Debug.Trace

getNewLoc :: S -> (Int, S)
getNewLoc (S varsMap loc funArgs semaphores queues) = (loc + 1, S varsMap (loc + 1) funArgs semaphores queues)

getNNewLocs :: S -> Int -> ([Int], S)
getNNewLocs s n = Prelude.foldl (\(l, s_) _ ->
    let
      (x, ns_) = getNewLoc s_
    in (x:l, ns_)
  ) ([], s) [1..n]

putInLoc :: Int -> (Bool, E, FType, FValueStatement) -> S -> S
putInLoc loc thing (S varsMap newIntLoc funArgs semaphores queues) =
  S (insert loc thing varsMap) newIntLoc funArgs semaphores queues

stateLookup :: Int -> S -> (Bool, E, FType, FValueStatement)
stateLookup loc (S varsMap _ _ _ _) = 
  if member loc varsMap
    then varsMap ! loc
    else trace ("loc not found in state " ++ show loc ++ "\nhaving vars: ") undefined

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

putQueue :: S -> QueueT -> S
putQueue (S varsMap newIntLoc functionArgsMap semaphores queues) thing =
  S varsMap newIntLoc functionArgsMap semaphores (queues ++ [thing])

anyAvailibleQueue :: S -> Bool
anyAvailibleQueue (S _ _ _ semaphores queues) = any (`notBlockedByAnyOfTheSemaphores` semaphores) queues

notBlockedByAnyOfTheSemaphores :: QueueT -> [SemaphoreT] -> Bool
notBlockedByAnyOfTheSemaphores (QueueT _ _ _ f y) [] = not $ f || y
notBlockedByAnyOfTheSemaphores q semaphores = any (q `notBlockedBySemaphoreOrNotFinishedOrNotYielding`) semaphores

checkNotBlocked :: QueueT -> S -> Bool
checkNotBlocked queue (S _ _ _ semaphores _) = notBlockedByAnyOfTheSemaphores queue semaphores

notBlockedBySemaphoreOrNotFinishedOrNotYielding :: QueueT -> SemaphoreT -> Bool
notBlockedBySemaphoreOrNotFinishedOrNotYielding (QueueT _ _ queueId b y) (SemaphoreT blockedQueues _ _) = queueId `notElem` blockedQueues && not b && not y

getAvailibleQueue :: S -> QueueT
getAvailibleQueue (S _ _ _ semaphores queues) = first (`notBlockedByAnyOfTheSemaphores` semaphores) queues

putInQueue :: Int -> QueueT -> S -> S
putInQueue queueId q (S varsMap newIntLoc functionArgsMap semaphores queues) =
  let
    updatedQueues = updateQueues queueId q queues
  in S varsMap newIntLoc functionArgsMap semaphores updatedQueues

updateQueues :: Int -> QueueT -> [QueueT] -> [QueueT]
updateQueues qId q [] = undefined
updateQueues qId q (QueueT e vs qId_ b y : queues) = 
  if qId == qId_ 
    then q:queues 
    else QueueT e vs qId_ b y : updateQueues qId q queues

getQueue :: Int -> S -> QueueT
getQueue qId state = first (\(QueueT _ _ id _ _) -> id == qId) $ queues state

getNewSemaphore :: S -> (SemaphoreT, S)
getNewSemaphore (S varsMap newIntLoc functionArgsMap semaphores queues) =
  let
    newSemId = length semaphores
  in (SemaphoreT [] 0 newSemId, S varsMap newIntLoc functionArgsMap (semaphores ++ [SemaphoreT [] 0 newSemId]) queues)

getSemaphore :: Int -> S -> SemaphoreT
getSemaphore semId (S _ _ _ semaphoreList _) = first (\(SemaphoreT _ _ i) -> i == semId) semaphoreList

putSemaphore :: Int -> SemaphoreT -> S -> S
putSemaphore semId sem (S varsMap newIntLoc functionArgsMap semaphoreList queueList) =
  let
    updatedSemaphores = updateSemaphores semId sem semaphoreList
  in S varsMap newIntLoc functionArgsMap updatedSemaphores queueList

updateSemaphores :: Int -> SemaphoreT -> [SemaphoreT] -> [SemaphoreT]
updateSemaphores semId s [] = undefined
updateSemaphores semId s (SemaphoreT blockedQueues semValue semId_ : semaphores) = 
  if semId == semId_ 
    then s:semaphores 
    else SemaphoreT blockedQueues semValue semId_ : updateSemaphores semId s semaphores

yieldQueue :: Int -> S -> S
yieldQueue queueId state =
  let
    (QueueT e vs _ b _) = getQueue queueId state
  in putInQueue queueId (QueueT e vs queueId b True) state

unyieldQueue :: Int -> S -> S
unyieldQueue queueId state =
  let
    (QueueT e vs _ b _) = getQueue queueId state
  in putInQueue queueId (QueueT e vs queueId b False) state

getNewState :: S
getNewState = S empty 0 empty [] []