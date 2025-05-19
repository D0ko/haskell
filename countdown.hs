module Countdown where


import Data.Ord (comparing)

import qualified Data.Foldable as F
import qualified Data.List     as L

-- Inner type
type Value = Int

-- Arithmetic expression
data AExpr = Num Value | App BOp AExpr AExpr

-- Binary operator
data BOp = Add | Sub | Mul | Div

-- Computation (i.e., valued arithmetic expression)
newtype VAExpr = VAExpr (AExpr,Value)

instance Show BOp where
  show Add = "+"
  show Sub = "-"
  show Mul = "x"
  show Div = "/"

instance Show AExpr where
  show (Num x) = show x
  show (App op e1 e2) = "("      ++
                        show e1  ++
                        " "      ++
                        show op  ++
                         " "     ++
                        show e2  ++
                        ")"

instance Show VAExpr where
  show (VAExpr (e,v)) = show e ++ " = " ++ show v


sublists :: [a] -> [[a]]
sublists [] = []
sublists (x:xs) = [x:ys | ys <- sublists xs] ++ sublists xs ++ [[x]]

legal1 :: BOp -> Value -> Value -> Bool
legal1 Sub x y = x > y
legal1 Div x y = y /= 0 && x `mod` y == 0
legal1 _ _ _ = True 

apply :: BOp -> Value -> Value -> Value
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y

value :: AExpr -> Value
value (Num x) = x
value (App op e1 e2) = apply op (value e1) (value e2)

unmerges1 :: [a] -> [([a], [a])]
unmerges1 [x, y] = [([x], [y]), ([y], [x])]
unmerges1 (x:xs) = ([x], xs) : (xs, [x]) : concatMap cons2 (unmerges1 xs)
    where
        cons2 (ys, zs) = [(x:ys, zs), (ys, x:zs)]


combineVAExprs1 :: VAExpr -> VAExpr -> [VAExpr]
combineVAExprs1 (VAExpr (e1, v1)) (VAExpr (e2, v2)) = [mk op | op <- ops, legal1 op v1 v2]
    where
        ops = [Add, Sub, Mul, Div]
        mk op = VAExpr (App op e1 e2, apply op v1 v2)
--combineVAExprs1 (VAExpr (e1,v1)) (VAExpr (e2,v2)) = [(VAExpr (App op e1 e2, val)) | op <- [Add, Sub, Mul, Div], let val = apply op v1 v2, legal1 op v1 v2]


mkAExprs1 :: [Int] -> [VAExpr]
mkAExprs1 [] = []
mkAExprs1 [x] = [VAExpr (Num x, x)]
mkAExprs1 xs = concatMap combineExprs (unmerges1 xs)
    where combineExprs (xs', xs'') = [va | va1 <- mkAExprs1 xs', va2 <- mkAExprs1 xs'', va <- combineVAExprs1 va1 va2]

searchBest :: Value -> [VAExpr] -> VAExpr
searchBest x (e:es) = go e es
  where
    go best [] = best
    go best@(VAExpr (_, v1)) (e@(VAExpr (_, v2)) : es)
      | abs (x - v1) == 0 = best
      | abs (x - v2) < abs (x - v1) = go e es
      | otherwise = go best es
{- searchBest target exprs = L.minimumBy (comparing diff) exprs
    where
        diff (VAExpr (_, v)) = abs (target - v)
 -}


countdown1 :: Int -> [Int] -> VAExpr
countdown1 x xs = searchBest x $ mkAExprs1 xs