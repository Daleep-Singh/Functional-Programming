{-|
Module: A2
Description: Assignment 2
Copyright: (c) University of Toronto Mississagua
               CSC324 Principles of Programming Languages, Fall 2021
-}
-- This lists what this module exports. Don't change this!
module A2
  (
    runStag,
    eval
  )
where

-- You *may not* add imports from Data.Map, or any other imports
import A2Types(Expr(..), Value(..), Env)
import qualified Data.Map (lookup, insert, empty)

--Application helper
helper :: Env -> [String] -> [Value] -> Env
helper env [] [] = env
helper env (x:xs) (y:ys) = helper (Data.Map.insert x y env) xs ys

-- | Runs a StagShell expression by calling `eval` with the empty environment
runStag :: Expr -> Value
runStag e = eval Data.Map.empty e


-- | An interpreter for the StagShell language.
eval :: Env -> Expr -> Value
eval env (Literal v) = v

eval env (Plus a b)  = case ((eval env a), (eval env b)) of
    (Num x, Num y)       -> (Num (x + y)) 
    (Error t, z)         -> (Error t)
    (t, Error z)         -> (Error z)
    (t,z)                -> (Error "Plus")

eval env (Times a b) = case ((eval env a), (eval env b)) of
    (Num x, Num y)      -> (Num(x * y))
    (Error t, z)         -> (Error t)
    (t, Error z)         -> (Error z)
    (t,z)                -> (Error "Times")


eval env (Equal a b) = case ((eval env a),(eval env b)) of
    (Error x, y)  -> (Error x)
    (x, Error y)  -> (Error y)
    (x,y) -> if (x ==y) then T else F

eval env (Cons a b) = case ((eval env a), (eval env b)) of  
    (Error t, _)        -> (Error t)  
    (_, Error z)        -> (Error z) 
    (x,y)               -> (Pair x y) 


eval env (First a) = case ((eval env a)) of
    (Pair x y) -> (x)
    (z) -> (Error "First")

eval env (Rest a) = case ((eval env a)) of
    (Pair x y) -> (y)
    (z) -> (Error "Rest")

eval env (If a b c) = case ((eval env a)) of  
    (T) -> ((eval env b))  
    (F) -> ((eval env c))  
    (Error z) -> (Error z)

eval env (Var name)  = case (Data.Map.lookup name env) of
    Just a  -> (a) -- "a" is of type Value 
    Nothing -> (Error "Var") -- "name" is not found in "env"

--Closure
eval env (Lambda fnparam fnbody) = (Closure fnparam env fnbody)

eval env (App fnExpr argExprs) = case ((eval env fnExpr)) of
    (Closure fnparam fnenv fnbody) -> if length fnparam /= length argExprs then (Error "App") else
      let 
      newenv = helper env fnparam (map (\x -> eval fnenv x) argExprs) --map is to make sure all argexprs evaluate to Literal values.
      in (eval newenv fnbody) --actual fn application 
    (z) -> (Error "App")


h :: [Value] -> Bool
h [] = False
h (x:xs) = case (x) of
  (Pair x y) -> True
  (x) -> h(xs)











