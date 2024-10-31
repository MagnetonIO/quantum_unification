module Main where

import QuantumDecoherence
import QuantumErrorCorrection
import UnifiedEquation

main :: IO ()
main = do
  putStrLn "Running Haskell Quantum Unification Code"
  testQuantumDecoherence
  testQuantumErrorCorrection
  testUnifiedEquation
