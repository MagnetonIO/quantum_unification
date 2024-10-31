module TimeDilation where

-- Special relativistic time dilation
specialRelativityDilation :: Double -> Double -> Double
specialRelativityDilation v t = t / sqrt (1 - (v * v / c2))
  where c2 = 299792458 ^ 2  -- Speed of light squared

-- Gravitational time dilation
gravitationalDilation :: Double -> Double -> Double
gravitationalDilation phi t = t * sqrt (1 - 2 * phi / c2)
  where c2 = 299792458 ^ 2  -- Speed of light squared

-- Test function for time dilation
testTimeDilation :: IO ()
testTimeDilation = do
  let t = 1.0  -- Time in seconds
  let v = 100000  -- Velocity in meters per second
  let phi = 9.8   -- Gravitational potential
  putStrLn $ "Special Relativity Dilation: " ++ show (specialRelativityDilation v t)
  putStrLn $ "Gravitational Dilation: " ++ show (gravitationalDilation phi t)
