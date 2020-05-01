module Util.Util where

import Debug.Trace

dList :: (Show a, Show b) => [a] -> [b] -> [(a, b)]
dList [] _ = []
dList (x:xs) (y:ys) = (x, y):dList xs ys
dList l1 l2 = trace (show l1 ++ show l2) undefined

tList :: [a] -> [b] -> [c] -> [(a, b, c)]
tList [] [] [] = []
tList (x:xs) (y:ys) (z:zs) = (x, y, z):tList xs ys zs
tList _ _ _ = undefined

andL :: [Bool] -> Bool
andL = foldr (&&) True