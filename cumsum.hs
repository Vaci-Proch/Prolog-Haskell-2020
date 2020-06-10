cumsum :: Num a => [[a]] -> [[a]]
cumsum list = scanl1 (\ xx yy -> (zipWith (\ xxx yyy -> (xxx + yyy)) xx yy)) (map (\ x -> scanl1 (+) x) list)