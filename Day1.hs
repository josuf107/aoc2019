main = do
    result1 <- sum . fmap (calculateFuel . read) . lines <$> readFile "input1.txt"
    print result1
    result2 <- sum . fmap (calculateFuelRecursive . read) . lines <$> readFile "input1.txt"
    print result2

calculateFuel :: Int -> Int
calculateFuel mass = mass `div` 3 - 2

calculateFuelRecursive :: Int -> Int
calculateFuelRecursive mass = let fuel = calculateFuel mass in if fuel < 0 then 0 else fuel + calculateFuelRecursive fuel
