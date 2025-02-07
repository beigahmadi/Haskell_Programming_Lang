-- Mahdi Beigahmadi
-- Student ID: 301570853
-- CMPT383, Programming Assignmnet 1
-- Feb 2025

import Data.List (nub)
import Data.Map.Strict qualified as Map
import System.Environment (getArgs)

type VarId = String

type VarAsgn = Map.Map VarId Bool

data Prop
  = Const Bool
  | Var VarId
  | Not Prop
  | And Prop Prop
  | Or Prop Prop
  | Imply Prop Prop
  | Iff Prop Prop
  deriving (Eq, Read, Show)

-- Step 1
findVarIds :: Prop -> [VarId]
findVarIds (Const _) = []
findVarIds (Var v) = [v]
findVarIds (Not p) = findVarIds p
findVarIds (And p q) = nub (findVarIds p ++ findVarIds q)
findVarIds (Or p q) = nub (findVarIds p ++ findVarIds q)
findVarIds (Imply p q) = nub (findVarIds p ++ findVarIds q)
findVarIds (Iff p q) = nub (findVarIds p ++ findVarIds q)

-- Step 2
genVarAsgns :: [VarId] -> [VarAsgn]
genVarAsgns [] = [Map.empty]
genVarAsgns (v : vs) =
  [ Map.insert v a assignedVar | a <- [True, False], assignedVar <- genVarAsgns vs
  ]

-- Step 3
eval :: Prop -> VarAsgn -> Bool
eval (Const a) _ = a
eval (Var v) assignedVar = case Map.lookup v assignedVar of
  Just a -> a
  Nothing -> error ("Invalid  " ++ v ++ "! Not Found in Assigned Variables")
eval (Not p) assignedVar = not (eval p assignedVar)
eval (And p q) assignedVar = eval p assignedVar && eval q assignedVar
eval (Or p q) assignedVar = eval p assignedVar || eval q assignedVar
eval (Imply p q) assignedVar = not (eval p assignedVar) || eval q assignedVar
eval (Iff p q) assignedVar = eval p assignedVar == eval q assignedVar

-- Step 4
sat :: Prop -> Bool
sat satisfied = any (eval satisfied) (genVarAsgns (findVarIds satisfied))

-- Step 5
readFormula :: String -> Prop
readFormula = read

-- Step 6
checkFormula :: String -> String
checkFormula f
  | sat (readFormula f) = "SAT"
  | otherwise = "UNSAT"

-- Step 7
main :: IO ()
main = do
  arg <- getArgs
  case arg of
    [] -> putStrLn ""
    (file : _) -> do
      contents <- readFile file
      let formulas = lines contents
      let resultsOfTable = map checkFormula formulas
      mapM_ putStrLn resultsOfTable