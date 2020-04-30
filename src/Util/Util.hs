module Util.Util where

dList :: [a] -> [b] -> [(a, b)]
dList [] [] = []
dList (x:xs) (y:ys) = (x, y):(dList xs ys)
dList _ _ = undefined

tList :: [a] -> [b] -> [c] -> [(a, b, c)]
tList [] [] [] = []
tList (x:xs) (y:ys) (z:zs) = (x, y, z):(tList xs ys zs)
tList _ _ _ = undefined

andL :: [Bool] -> Bool
andL = foldr (&&) True