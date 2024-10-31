module OperatorTheory where

-- Define a linear operator
data Operator = Operator (Double -> Double)

-- Apply an operator to a quantum state (a function)
applyOperator :: Operator -> (Double -> Double) -> Double -> Double
applyOperator (Operator op) psi x = op (psi x)

-- Example of a quantum operator (scaling)
scaleOperator :: Double -> Operator
scaleOperator scalar = Operator (* scalar)

-- Test function for Operator Theory
testOperatorTheory :: IO ()
testOperatorTheory = do
  let psi = \x -> x * x
  let op = scaleOperator 2.0
  print $ applyOperator op psi 3.0  -- Should scale the result by 2
