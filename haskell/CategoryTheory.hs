module CategoryTheory where

-- Defining an object as an abstract type
data Object = Object String deriving (Show, Eq)

-- A morphism is a function between objects
data Morphism = Morphism Object Object (Object -> Object)

-- Functor: A mapping between categories
data Functor = Functor {
  fmapObj :: Object -> Object,
  fmapMor :: Morphism -> Morphism
}

-- Identity morphism
identity :: Object -> Morphism
identity o = Morphism o o id

-- Composition of morphisms
compose :: Morphism -> Morphism -> Maybe Morphism
compose (Morphism _ _ f) (Morphism o2 o3 g) = Just (Morphism o2 o3 (f . g))

-- Test for Category Theory
testCategoryTheory :: IO ()
testCategoryTheory = do
  let objA = Object "A"
  let objB = Object "B"
  let morphism = Morphism objA objB (\_ -> objB)
  let functor = Functor id (\m -> m)
  putStrLn $ "Test Functor: " ++ show (fmapObj functor objA)
