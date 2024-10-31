module ToposTheory where

-- A simple Sheaf structure where sections depend on a spacetime point
data Sheaf a = Sheaf [(Double, a)] deriving (Show)

-- Defining a section of the sheaf over a point in spacetime
sectionAt :: Sheaf a -> Double -> Maybe a
sectionAt (Sheaf sections) point = lookup point sections

-- A functor on sheaves
sheafFunctor :: (a -> b) -> Sheaf a -> Sheaf b
sheafFunctor f (Sheaf sections) = Sheaf (map (\(x, a) -> (x, f a)) sections)

-- Test function for Topos Theory
testToposTheory :: IO ()
testToposTheory = do
  let spacetimeSheaf = Sheaf [(0.0, "State1"), (1.0, "State2")]
  print $ sectionAt spacetimeSheaf 0.0
